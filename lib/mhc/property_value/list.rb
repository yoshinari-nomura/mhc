module Mhc
  module PropertyValue
    class List < Base
      include Enumerable

      ITEM_SEPARATOR = " "

      def initialize(item_class)
        @value = []
        @item_class = item_class
      end

      def each
        @value.each do |value|
          yield value
        end
      end

      def include?(o)
        @value.include?(o)
      end

      def empty?
        @value.empty?
      end

      def parse(string)
        string.strip.split(ITEM_SEPARATOR).each do |str|
          item = @item_class.parse(str)
          @value << item if item
        end
        return self
      end

      def to_mhc_string
        @value.map{|item| item.to_mhc_string}.join(ITEM_SEPARATOR)
      end
      alias_method :to_s, :to_mhc_string

    end # class List
  end # module PropertyValue
end # module Mhc
