module Mhc
  module PropertyValue
    class Period < Base

      UNIT2MIN = {'minute' => 1, 'hour' => 60, 'day' => 60*24}
      UNITS    = UNIT2MIN.keys
      REGEXP   = /(\d+)\s*(#{UNITS.join("|")})s?/

      def self.parse(string)
        return new.parse(string)
      end

      def parse(string)
        if REGEXP =~ string
          @minutes = (UNIT2MIN[$2] * $1.to_i)
        end
        return self
      end

      def to_mhc_string
        return "" unless @minutes

        value, unit_size, unit_name = @minutes, 1, "minute"

        UNIT2MIN.each do |unit,minutes|
          if @minutes % minutes == 0
            value = @minutes / minutes
            unit_size = minutes
            unit_name = unit
          end
        end
        return "#{value} #{unit_name}" + (value > 1 ? "s" : "")
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
