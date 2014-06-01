module Mhc
  module PropertyValue
    class Range < Base
      include Comparable
      ITEM_SEPARATOR = "-"

      attr_reader :first, :last

      def initialize(item_class, prefix = nil, first = nil, last = nil)
        @item_class, @prefix = item_class, prefix
        @first, @last = first, last
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
        return self.class.new(@item_class, @prefix, @first, @last)
      end

      def to_a
        array = []
        i = first
        while i <= last
          array << i
          i = i.succ
        end
        return array
      end

      def each
        i = first
        while i <= last
          yield(i)
          i = i.succ
        end
      end

      def narrow(from, to)
        from = @first if from.nil? or (@first and from < @first)
        to   = @last  if to.nil?   or (@last  and to   > @last)
        self.class.new(@item_class, @prefix, from, to)
      end

      def <=>(o)
        o = o.first if o.respond_to?(:first)
        safe_comp(self.first, o)
      end

      def infinit?
        return @first.nil? || @last.nil?
      end

      def blank?
        @first.nil? && @last.nil?
      end

      def to_mhc_string
        first = @first.nil? ? "" : @first.to_mhc_string
        last  = @last.nil?  ? "" : @last.to_mhc_string

        if first == last
          return @prefix.to_s + first
        else
          return @prefix.to_s + [first, last].join(ITEM_SEPARATOR)
        end
      end

      alias_method :to_s, :to_mhc_string

      private

      def cover?(item)
        return false if @first && item < @first
        return false if @last  && item > @last
        return true
      end

      def safe_comp(a, o)
        # nil is minimum
        return (a <=> o) if  a and  o
        return -1        if !a and  o
        return  1        if  a and !o
        return  0        if !a and !o
      end

    end # class Range
  end # module PropertyValue
end # module Mhc
