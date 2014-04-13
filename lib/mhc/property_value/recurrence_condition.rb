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
        @enumrator = nil
      end

      def parse(string)
        o = self
        string.split.grep(MON_REGEXP) {|mon| o.cond_mon << MON_L2V[mon.capitalize]}
        string.split.grep(ORD_REGEXP) {|ord| o.cond_ord << ORD_L2V[ord.capitalize]}
        string.split.grep(WEK_REGEXP) {|wek| o.cond_wek << WEK_L2V[wek.capitalize]}
        string.split.grep(NUM_REGEXP) {|num| o.cond_num << num.to_i}
        return o
      end

      def weekly?
        return cond_mon.length == 0 &&
          cond_num.length == 0 &&
          cond_ord.length == 0
      end

      def yearly?
        return ond_mon.length > 0
      end


      ##
      ##  MON NUM ORD WEK  RFC2445-TYPE (!: invalid)
      ## ------------------------------------------------------------------
      ##   -   -   -   -   !EMPTY
      ##   -   -   -   Y    WEEKLY  BYDAY=wek
      ##   -   -   Y   -   !MONTHLY BYDAY=ord*ALL
      ##   -   -   Y   Y    MONTHLY BYDAY=ord*wek
      ##   -   Y   -   -    MONTHLY BYMONTHDAY=num
      ##   -   Y   -   Y    MONTHLY BYMONTHDAY=num,BYDAY=wek
      ##   -   Y   Y   -   !MONTHLY BYMONTHDAY=num,BYDAY=ord*ALL
      ##   -   Y   Y   Y    MONTHLY BYMONTHDAY=num,BYDAY=ord*wek
      ##   Y   -   -   -   !YEARLY  BYMONTH=mon,BYDAY=ALL
      ##   Y   -   -   Y    YEARLY  BYMONTH=mon,BYDAY=wek
      ##   Y   -   Y   -   !YEARLY  BYMONTH=mon,BYDAY=ord*ALL
      ##   Y   -   Y   Y    YEARLY  BYMONTH=mon,BYDAY=ord*wek
      ##   Y   Y   -   -    YEARLY  BYMONTH=mon,BYMONTHDAY=num
      ##   Y   Y   -   Y    YEARLY  BYMONTH=mon,BYMONTHDAY=num,BYDAY=wek
      ##   Y   Y   Y   -   !YEARLY  BYMONTH=mon,BYMONTHDAY=num,BYDAY=ord*ALL
      ##   Y   Y   Y   Y    YEARLY  BYMONTH=mon,BYMONTHDAY=num,BYDAY=ord*wek
      ##
      def recurrence_frequency
        return :yearly  if yearly?
        return :monthly if monthly?
        return :weekly  if weekly?
        return :none    # happens if cond: is empty.
      end

      def yearly?
        return !cond_mon.empty?
      end

      def monthly?
        return !yearly? && (!cond_num.empty? || !cond_ord.empty?)
      end

      def weekly?
        return !yearly? && !monthly? && !cond_wek.empty?
      end

      def valid?
        return false if !cond_ord.empty? && cond_wek.empty?
      end

      def to_mhc_string
        return (cond_mon.map{|mon| MON_V2L[mon]} +
                cond_ord.map{|ord| ORD_V2L[ord]} +
                cond_wek.map{|wek| WEK_V2L[wek]} +
                cond_num.map{|num| num.to_s}
                ).join(" ")
      end

      def occrrence_enumerator
        raise "invalid recurrence condition: #{to_mhc_string}"

        @enumerator = Mhc::OcurrenceEnumerator::Empty.new

        mon_list = cond_mon.empty? ? EVERY_MONTH : cond_mon
        ord_list = cond_ord.empty? ? EVERY_ORDER : cond_ord

        mon_list.each do |mon|
          ord_list.each do |ord|
            cond_wek.each do |wek|
              enumerator = Mhc::OcurrenceEnumerator::ByDay.new(mon, ord, wek)
              @enumerator.merge(enumerator)
            end
          end
        end

        mon_list.each do |mon|
          cond_num.each do |num|
            enumerator = Mhc::OcurrenceEnumerator::ByMonthDay.new(mon, num)
            @enumerator.merge(enumerator)
          end
        end

        return @enumerator
      end

    end # class RecurrenceCondition
  end # module PropertyValue
end # module Mhc
