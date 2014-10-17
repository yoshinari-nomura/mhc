module Mhc
  module PropertyValue
    class RecurrenceCondition < Base

      # :stopdoc:
      MON_LABEL = %w(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec)
      MON_VALUE = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
      MON_L2V   = Hash[*MON_LABEL.zip(MON_VALUE).flatten]
      MON_V2L   = MON_L2V.invert

      ORD_LABEL = %w(1st 2nd 3rd 4th 5th Last)
      ORD_VALUE = [1, 2, 3, 4, 5, -1]
      ORD_L2V   = Hash[*ORD_LABEL.zip(ORD_VALUE).flatten]
      ORD_V2L   = ORD_L2V.invert

      WEK_LABEL = %w(Sun Mon Tue Wed Thu Fri Sat)
      WEK_VALUE = [0, 1, 2, 3, 4, 5, 6]
      WEK_L2V   = Hash[*WEK_LABEL.zip(WEK_VALUE).flatten]
      WEK_V2L   = WEK_L2V.invert
      WEK_V2I   = Hash[*WEK_VALUE.zip(%w(SU MO TU WE TH FR SA)).flatten]

      MON_REGEXP = /^#{MON_LABEL.join('|')}$/oi
      ORD_REGEXP = /^#{ORD_LABEL.join('|')}$/oi
      WEK_REGEXP = /^#{WEK_LABEL.join('|')}$/oi
      NUM_REGEXP = /^\d+$/oi
      # :startdoc:

      def cond_mon; return @cond_mon; end
      def cond_ord; return @cond_ord; end
      def cond_wek; return @cond_wek; end
      def cond_num; return @cond_num; end

      def initialize
        @cond_mon, @cond_ord, @cond_wek, @cond_num = [], [], [], []
      end

      def parse(string)
        o = self
        string.split.grep(MON_REGEXP) {|mon| o.cond_mon << MON_L2V[mon.capitalize]}
        string.split.grep(ORD_REGEXP) {|ord| o.cond_ord << ORD_L2V[ord.capitalize]}
        string.split.grep(WEK_REGEXP) {|wek| o.cond_wek << WEK_L2V[wek.capitalize]}
        string.split.grep(NUM_REGEXP) {|num| o.cond_num << num.to_i}
        return o
      end

      #--
      #  MON NUM ORD WEK  RFC2445-TYPE (!: invalid)
      # ------------------------------------------------------------------
      #   -   -   -   -   ! EMPTY
      #   -   -   -   Y     WEEKLY  BYDAY=wek
      #   -   -   Y   -   ! MONTHLY BYDAY=ord*ALL
      #   -   -   Y   Y     MONTHLY BYDAY=ord*wek
      #   -   Y   -   -     MONTHLY BYMONTHDAY=num
      #   -   Y   -   Y     MONTHLY BYMONTHDAY=num,BYDAY=wek
      #   -   Y   Y   -   ! MONTHLY BYMONTHDAY=num,BYDAY=ord*ALL
      #   -   Y   Y   Y     MONTHLY BYMONTHDAY=num,BYDAY=ord*wek
      #   Y   -   -   -   ! YEARLY  BYMONTH=mon,BYDAY=ALL
      #   Y   -   -   Y     YEARLY  BYMONTH=mon,BYDAY=wek
      #   Y   -   Y   -   ! YEARLY  BYMONTH=mon,BYDAY=ord*ALL
      #   Y   -   Y   Y     YEARLY  BYMONTH=mon,BYDAY=ord*wek
      #   Y   Y   -   -     YEARLY  BYMONTH=mon,BYMONTHDAY=num
      #   Y   Y   -   Y     YEARLY  BYMONTH=mon,BYMONTHDAY=num,BYDAY=wek
      #   Y   Y   Y   -   ! YEARLY  BYMONTH=mon,BYMONTHDAY=num,BYDAY=ord*ALL
      #   Y   Y   Y   Y     YEARLY  BYMONTH=mon,BYMONTHDAY=num,BYDAY=ord*wek
      #++
      def frequency
        return :none    if empty?
        return :daily   if daily?
        return :weekly  if weekly?
        return :monthly if monthly?
        return :yearly  if yearly?
      end

      def daily?
        false
      end

      def weekly?
        !yearly? && !monthly? && !cond_wek.empty?
      end

      def monthly?
        !yearly? && (!cond_num.empty? || !cond_ord.empty?)
      end

      def yearly?
        !cond_mon.empty?
      end

      def valid?
        frequency != :none
      end

      def empty?
        [@cond_mon, @cond_ord, @cond_wek, @cond_num].all?{|cond| cond.empty?}
      end

      # convert RRULE to X-SC-Cond:
      #
      # Due to the over-killing complexity of iCalendar (RFC5545)
      # format, converting RRULE to X-SC-* format has some restrictions:
      #
      # * Not allowed elements:
      #   * BYSECOND
      #   * BYMINUTE
      #   * BYHOUR
      #   * COUNT
      #   * BYYEARDAY (-366 to 366)
      #   * BYWEEKNO  (-53  to  53)
      #   * BYSETPOS  (-366 to 366)
      #   * Recurrence-ID (not part of RRULE)
      #
      # * Restricted elements:
      #
      #   * INTERVAL:
      #     * it should be 1
      #
      #   * BYMONTHDAY:
      #     * it should be (1..31)
      #
      #   * WKST:
      #     * it should be MO
      #
      #   * FREQ:
      #     * should be one of WEEKLY, MONTHLY, YEARLY
      #     * should be MONTHLY if BYDAY has (1|2|3|4|-1)
      #     * should be WEEKLY if BYDAY does not have (1|2|3|4|-1)
      #
      #   * BYDAY:
      #     * should be a list of (1|2|3|4|-1)?(MO|TU|WE|TH|FR|SA|SU)
      #
      #     * Every week should have the same number-prefix set:
      #       WE,SU           is OK => Wed Sun
      #       3WE,3SU         is OK => 3rd Wed Sun
      #       2WE,3WE,2SU,3SU is OK => 2nd 3rd Sun Wed
      #       3WE,2SU         is NG
      #       3WE,SU          is NG
      #
      # * Fully converted elements:
      #
      #   * UNTIL
      #     * YYYYMMDD should goes to X-SC-Duration: -YYYYMMDD
      #
      #   * BYMONTH
      #     * (1..12)* => (Jan|Feb|Mar|Jul|Aug|Sep|Oct|Nov|Dec)*
      #
      def validate_rrule(rrule)
        return true if rrule.to_s == ""
        return 1 if rrule =~ /(BYSECOND|BYMINUTE|BYHOUR|COUNT|BYYEARDAY|BYWEEKNO|BYSETPOS)/i
        return 2 if rrule =~ /INTERVAL=(\d+)/i and $1.to_i != 1
        return 3 if rrule =~ /BYMONTHDAY=([^;]+)/i and $1.split(",").map(&:to_i).any?{|i| i < 1 or i > 31}
        return 4 if rrule =~ /WKST=([^;]+)/i and $1 !~ /MO/
        return 5 if rrule =~ /FREQ=([^;]+)/i   and $1 !~ /WEEKLY|MONTHLY|YEARLY/i
        return 6 if rrule =~ /BYDAY=([^;]+)/i  and $1 =~ /\d/ and rrule !~ /FREQ=MONTHLY/i
        return 7 if rrule =~ /BYDAY=([^;]+)/i  and $1 !~ /\d/ and rrule !~ /FREQ=WEEKLY/i
        return 8 if rrule =~ /BYDAY=([^;]+)/i  and $1 !~ /((1|2|3|4|-1)?(MO|TU|WE|TH|FR|SA|SU))+/i
      end

      def set_from_ics(rrule)
        validate_rrule(rrule)

        cond_mon = []
        if rrule =~ /BYMONTH=([^;]+)/
          $1.split(",").each do |mon|
            cond_mon << mon.to_i
          end
        end

        cond_ord = []
        cond_wek = []
        week = {}
        if rrule =~ /BYDAY=([^;]+)/
          $1.scan(/(1|2|3|4|-1)?(MO|TU|WE|TH|FR|SA|SU)/).each do |o,w|
            week[w] ||= []
            week[w] << o.to_i # unpefixed week is replaced as 0
          end

          # Every week should have the same number-prefix set:
          return 9 unless week.values.all?{|orders| orders.sort == week.values.first.sort}

          order = week.values.first.sort
          #     * Number-prefixed week cannot coexist with unprefixed week
          #       WE,SU  is OK => Wed Sun
          #       WE,3SU is NG
          return 10 if order.length > 1 and order.member?(0) # 0 means non-numberd prefix

          order.delete(0)
          cond_ord = order

          week.each do |w, o|
            cond_wek << WEK_V2I.invert[w]
          end
        end

        cond_num = []
        if rrule =~ /BYMONTHDAY=([^;]+)/i
          $1.split(",").each do |n|
            cond_num << n.to_i
          end
        end
        @cond_mon, @cond_ord, @cond_wek, @cond_num = cond_mon, cond_ord, cond_wek, cond_num
        return self
      end

      def to_mhc_string
        return (cond_mon.map{|mon| MON_V2L[mon]} +
                cond_ord.map{|ord| ORD_V2L[ord]} +
                cond_wek.map{|wek| WEK_V2L[wek]} +
                cond_num.map{|num| num.to_s}
                ).join(" ")
      end

      def to_ics(dtstart = nil, until_date = nil)
        return nil unless valid?

        ord_wek = (cond_ord.empty? ? [""] : cond_ord).product(cond_wek)
        day = ord_wek.map {|o,w| o.to_s + WEK_V2I[w] }.join(',')

        if until_date
          if dtstart.respond_to?(:hour)
            tz = TZInfo::Timezone.get(ENV["MHC_TZID"] || 'UTC')
            localtime = Mhc::PropertyValue::Time.new.parse(dtstart.strftime("%H:%M")).to_datetime(until_date).to_time
            until_str = tz.local_to_utc(localtime).strftime("%Y%m%dT%H%M%SZ")
            # puts "until_str local (tz=#{tz.name}) : #{localtime.strftime("%Y%m%dT%H%M%S")} utc: #{until_str}"
          else
            until_str = until_date.strftime("%Y%m%d")
          end
        end

        ics = "FREQ=#{frequency.to_s.upcase};INTERVAL=1;WKST=MO"

        ics += ";BYMONTH=#{cond_mon.join(',')}"    unless cond_mon.empty?
        ics += ";BYDAY=#{day}"                     unless day.empty?
        ics += ";BYMONTHDAY=#{cond_num.join(',')}" unless cond_num.empty?
        ics += ";UNTIL=#{until_str}" unless until_date.nil?

        return ics
      end

    end # class RecurrenceCondition
  end # module PropertyValue
end # module Mhc
