module Mhc
  module PropertyValue
    class Time < Base
      include Comparable

      def parse(string)
        if /^(\d+):(\d+)$/ =~ string
          @sec = ($1.to_i) * 3600 + ($2.to_i) * 60
        end
        return self
      end

      def days;    (@sec        ) / 86400 ;end
      def hour;    (@sec % 86400) / 3600  ;end
      def minute;  (@sec % 3600)  / 60    ;end

      def <=>(o)
        return @sec <=> o.to_i
      end

      def to_mhc_string
        return format("%02d:%02d", hour, minute)
      end

      alias_method :to_s, :to_mhc_string

      def to_i
        return @sec
      end

      def to_a
        return [hour, minute]
      end

      def to_time(date = Mhc::PropertyValue::Date.new(1970, 1, 2))
        date = date + days
        ::Time.local(date.year, date.month, date.day, hour, minute)
      end
    end
  end # module PropertyValue
end # module Mhc
