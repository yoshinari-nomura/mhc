module Mhc
  module PropertyValue
    class Period < Base
      UNIT_TO_MIN = {'minute' => 1, 'hour' => 60, 'day' => 60*24}
      UNITS       = UNIT_TO_MIN.keys
      REGEXP      = /(\d+)(#{UNITS.join("|")})s?/o

      def initialize
        @minutes = 0
      end

      def self.parse(string)
        return new.parse(string)
      end

      def parse(string)
        if REGEXP =~ string
          @minutes = (UNIT_TO_MIN[$2] * $1.to_i)
        end
        return self
      end

      def in_minutes
        return @minutes
      end

      def to_mhc
        return "" unless @minutes

        value, unit_size = @minutes, 1

        UNIT_TO_MIN.each do |unit,minutes|
          if @minutes % minutes == 0 && minutes <= unit_size
            value = @minutes / minutes
            unit_size = minutes
            unit_name = unit
          end
        end
        return "#{value} #{unit_name}"
      end

      def alarm_trigger
        duration = nil
        if @alarm
          seconds = @alarm
          duration = "-P"
          duration += "#{seconds /= 86400}D" if seconds >= 86400
          duration += "T#{seconds /= 3600}H" if seconds >= 3600
          duration += "T#{seconds /= 60}M"   if seconds >= 60
        end
        return duration
      end

    end # class Period
  end # module PropertyValue
end # module Mhc
