module Mhc
  module PropertyValue
    class Integer < Base
      def parse(string)
        @value = string.to_i if /^\d+$/ =~ string
        return self
      end

      def to_i
        @value.to_i
      end

    end # class Integer
  end # module PropertyValue
end # module Mhc
