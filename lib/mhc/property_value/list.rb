module Mhc
  module PropertyValue
    class List < Base
      include Enumerable

      ITEM_SEPARATOR = " "

      def initialize(item_class, prefix = nil)
        @list = []
        @prefix = prefix
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

      def parse(string)
        string.strip.split(ITEM_SEPARATOR).each do |str|
          if @prefix and /^#{Regexp.escape(@prefix)}(.+)/ =~ str
            item = @item_class.parse($1)
          elsif not @prefix
            item = @item_class.parse(str)
          end
          @list << item if item
        end
        return self
      end

      def to_mhc_string
        @list.map{|item| @prefix.to_s + item.to_mhc_string}.join(ITEM_SEPARATOR)
      end

    end # class List
  end # module PropertyValue
end # module Mhc
