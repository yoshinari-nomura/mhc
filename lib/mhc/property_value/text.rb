module Mhc
  module PropertyValue
    class Text < Base
      require "nkf"
      def to_mhc_string
        return NKF.nkf("-w", @value.to_s)
      end
      alias_method :to_s, :to_mhc_string
    end # class Text
  end # module PropertyValue
end # module Mhc
