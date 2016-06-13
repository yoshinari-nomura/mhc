module Mhc
  class Formatter
    # prepare
    # format_header
    #   format_day_header
    #     format_item_header
    #     format_item
    #     format_item_hooter
    #   format_day_hooter
    # format_footer
    # teardown
    class Base
      def initialize(date_range:, options:nil)
        @date_range = date_range
        @options = options
        @occurrences, @events, @items = [], [], {}
        @event_hash = {}
      end

      def <<(occurrence)
        event = occurrence.event
        @occurrences << occurrence
        @events << event unless @event_hash[event]
        @event_hash[event] = true

        @items[occurrence.date] ||= []
        @items[occurrence.date] << occurrence
      end

      def to_s
        context = {:items => @items}.merge(@options)
        prepare(context)
        string = format_header(context) + format_body(context) + format_footer(context)
        teardown(context)
        return string
      end

      ################################################################
      private

      def prepare(context);  end
      def teardown(context); end

      def pad_empty_dates
        @date_range.each do |date|
          @items[date] ||= []
        end
      end

      def expand_multiple_days_occurrences
        @occurrences.each do |oc|
          next if oc.oneday?
          ((oc.first + 1) .. oc.last).each do |date|
            @items[date] ||= []
            @items[date] << oc
          end
        end
      end

      def format_header(context); ""; end
      def format_footer(context); ""; end
      def format_day_header(context, date, is_holiday); ""; end
      def format_day_footer(context, date); ""; end

      def format_body(context)
        context[:number] = 0
        @items.keys.sort.map{|date| format_day(context, date, @items[date]) }.join
      end

      def format_day(context, date, items)
        string = format_day_header(context, date, items.any?{|e| e.holiday?})

        items = sort_items_in_day(items)
        items.each_with_index do |occurrence, count|
          context[:number] += 1
          context[:number_in_day] = count + 1
          string += format_item(context, date, occurrence)
        end

        return string + format_day_footer(context, date)
      end

      def format_item(context, date, item)
        raise "Implement in subclasses."
      end

      def format_item_header(context, date, item)
        raise "Implement in subclasses."
      end

      ################################################################
      ## helpers
      def append(item, separator = " ")
        return "" if item.to_s.empty?
        return separator + item.to_s
      end

      def prepend(item, separator = " ")
        return "" if item.to_s.empty?
        return item.to_s + separator
      end

      def enclose(item, bracket = "[]")
        return "" if item.to_s.empty?
        return bracket[0] + item.to_s + bracket[1]
      end

      # sort occurrences in a day
      # make sure all-day occurrences are prior to others
      def sort_items_in_day(items)
        items.sort do |a,b|
          sign_a = a.allday? ? 0 : 1
          sign_b = b.allday? ? 0 : 1

          if sign_a != sign_b
            sign_a - sign_b
          else
            a <=> b
          end
        end
      end

    end # class Base
  end # module Formatter
end # module Mhc
