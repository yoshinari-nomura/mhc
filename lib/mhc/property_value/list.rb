module Mhc
  module PropertyValue
    class List < Base
      include Enumerable

      ITEM_SEPARATOR = " "

      def initialize(item_class)
        @list = []
        @item_class = item_class
      end

      def each
        @list.each do |value|
          yield value
        end
      end

      def include?(o)
        @list.include?(o)
      end

      def empty?
        @list.empty?
      end

      def parse(string)
        string.strip.split(ITEM_SEPARATOR).each do |str|
          item = @item_class.parse(str)
          @list << item if item
        end
        return self
      end

      def to_mhc_string
        @list.map{|item| item.to_mhc_string}.join(ITEM_SEPARATOR)
      end

    end # class List
  end # module PropertyValue
end # module Mhc
