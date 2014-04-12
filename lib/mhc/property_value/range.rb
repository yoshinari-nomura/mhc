module Mhc
  module PropertyValue
    class Range < Base
      ITEM_SEPARATOR = "-"

      def initialize(item_class)
        @item_class = item_class
        @first, @last = nil, nil
      end

      # our Range acceps these 3 forms:
      #   (1) A-B    : first, last = A, B
      #   (2) A      : first, last = A, A
      #   (3) A-     : first, last = A, nil
      #   (4) -B     : first, last = nil, B
      #
      # nil means range is open (infinite).
      #
      def parse(string)
        @first, @last = nil, nil
        first, last = string.split(ITEM_SEPARATOR, 2)
        last = first if last.nil? # single "A" means "A-A"

        @first = @item_class.parse(first) unless first.to_s == ""
        @last  = @item_class.parse(last)  unless last.to_s  == ""
        return self
      end

      def each
        i = first
        while i <= last
          yield(i)
          i = i.succ
        end
      end

      def include?(item)
        return false if @first && item < @first
        return false if @last  && item > @last
        return true
      end

      def <=>(o)
        # nil is minimum
        return self.first <=> o.first if self.first and o.first
        return -1 if !self.first and  o.first
        return  1 if  self.first and !o.first
        return  0 if !self.first and !o.first
      end

      def first
        return @first
      end

      def last
        return @last
      end

      def infinit?
        return @first.nil? || @last.nil?
      end

      def to_mhc_string
        first = @first.nil? ? "" : @first.to_mhc_string
        last  = @last.nil?  ? "" : @last.to_mhc_string

        if first == last
          return first
        else
          return [first, last].join(ITEM_SEPARATOR)
        end
      end

    end # class Range
  end # module PropertyValue
end # module Mhc
