module Mhc
  module PropertyValue
    class RecurrenceCondition < Base

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

      ##
      ##  MON NUM ORD WEK  RFC2445-TYPE (!: invalid)
      ## ------------------------------------------------------------------
      ##   -   -   -   -   ! EMPTY
      ##   -   -   -   Y     WEEKLY  BYDAY=wek
      ##   -   -   Y   -   ! MONTHLY BYDAY=ord*ALL
      ##   -   -   Y   Y     MONTHLY BYDAY=ord*wek
      ##   -   Y   -   -     MONTHLY BYMONTHDAY=num
      ##   -   Y   -   Y     MONTHLY BYMONTHDAY=num,BYDAY=wek
      ##   -   Y   Y   -   ! MONTHLY BYMONTHDAY=num,BYDAY=ord*ALL
      ##   -   Y   Y   Y     MONTHLY BYMONTHDAY=num,BYDAY=ord*wek
      ##   Y   -   -   -   ! YEARLY  BYMONTH=mon,BYDAY=ALL
      ##   Y   -   -   Y     YEARLY  BYMONTH=mon,BYDAY=wek
      ##   Y   -   Y   -   ! YEARLY  BYMONTH=mon,BYDAY=ord*ALL
      ##   Y   -   Y   Y     YEARLY  BYMONTH=mon,BYDAY=ord*wek
      ##   Y   Y   -   -     YEARLY  BYMONTH=mon,BYMONTHDAY=num
      ##   Y   Y   -   Y     YEARLY  BYMONTH=mon,BYMONTHDAY=num,BYDAY=wek
      ##   Y   Y   Y   -   ! YEARLY  BYMONTH=mon,BYMONTHDAY=num,BYDAY=ord*ALL
      ##   Y   Y   Y   Y     YEARLY  BYMONTH=mon,BYMONTHDAY=num,BYDAY=ord*wek
      ##
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

      def to_mhc_string
        return (cond_mon.map{|mon| MON_V2L[mon]} +
                cond_ord.map{|ord| ORD_V2L[ord]} +
                cond_wek.map{|wek| WEK_V2L[wek]} +
                cond_num.map{|num| num.to_s}
                ).join(" ")
      end

      def to_ics(until_date = nil)
        return nil unless valid?

        ord_wek = (cond_ord.empty? ? [""] : cond_ord).product(cond_wek)
        day = ord_wek.map {|o,w| o.to_s + WEK_V2I[w] }.join(',')

        ics = "FREQ=#{frequency.to_s.upcase};INTERVAL=1;WKST=MO"

        ics += ";BYMONTH=#{cond_mon.join(',')}"    unless cond_mon.empty?
        ics += ";BYDAY=#{day}"                     unless day.empty?
        ics += ";BYMONTHDAY=#{cond_num.join(',')}" unless cond_num.empty?
        ics += ";UNTIL=#{until_date.to_ics}"       unless until_date.nil?

        return ics
      end

    end # class RecurrenceCondition
  end # module PropertyValue
end # module Mhc
