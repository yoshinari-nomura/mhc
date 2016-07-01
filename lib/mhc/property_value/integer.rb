module Mhc
  module PropertyValue
    class Integer < Base
      def parse(string)
        if /^\d+$/ =~ string
          @value = string.to_i
        else
          raise ParseError, "invalid integer format \"#{string}\""
        end
        return self
      end

      def to_i
        @value.to_i
      end

    end # class Integer
  end # module PropertyValue
end # module Mhc
