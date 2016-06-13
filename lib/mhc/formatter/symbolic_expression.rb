module Mhc
  class Formatter
    class SymbolicExpression < Base
      private

      def format_header(context);  "(";   end
      def format_footer(context);  "(periods #{@periods}))\n"; end

      def format_day_header(context, date, is_holiday)
        date.strftime("((%2m %2d %Y) . (")
      end

      def format_item(context, date, item)
        unless item.oneday?
          format_multiple_days_item(context, date, item)
          return ""
        end
        format_item_line(item)
      end

      def format_multiple_days_item(context, date, item)
        @periods ||= ""
        @periods +=  item.first.strftime("((%2m %2d %Y) ") +
                    item.last.strftime(" (%2m %2d %Y) ") +
          format_item_line(item) + ') '
      end

      def format_day_footer(context, date); ")) "; end

      def format_item_line(item)
        '"' +
          format("%s%s%s",
                 prepend(item.time_range.first.to_s).toutf8,
                 item.subject.to_s.toutf8,
                 append(enclose(item.location)).toutf8).gsub(/[\"\\]/, '\\\\\&') +
          '" '

      end

    end # class SymbolicExpression
  end # class Formatter
end # module Mhc
