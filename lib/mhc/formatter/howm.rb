module Mhc
  class Formatter
    class Howm < Base
      private

      def format_header(context)
        format("= mhc %s--%s\n", *context[:items].keys.minmax)
      end

      def format_item(context, date, item)
        string = format("[%04d-%02d-%02d %5s]%1s %s\n",
                        date.year, date.month, date.mday,
                        item.time_range.first.to_s,
                        mark_todo(item.categories.to_mhc_string),
                        item.subject)
        if item.description.to_s != ""
          string += item.description.to_s.gsub(/^/, " ") + "\n"
        end
        return string
      end

      def mark_todo(category)
        case category
        when /done/i
          "."
        when /todo/i
          "+"
        else
          "@"
        end
      end

    end # class Howm
  end # class Formatter
end # module Mhc
