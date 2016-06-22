require "date"

module Mhc
  module PropertyValue
    class DateTime < ::DateTime

      def to_mhc_string
        return strftime("%Y%m%d/%H:%M")
      end

      def absolute_from_epoch
        return (self - Date.new(1970, 1, 1)).to_i
      end

      alias_method :to_s, :to_mhc_string

    end # class DateTime
  end # module PropertyValue
end # module Mhc
