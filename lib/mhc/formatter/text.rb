module Mhc
  class Formatter
    class Text < Base
      def prepare(context)
        expand_multiple_days_occurrences
      end

      def format_item(context, date, item)
        subject = item.subject.to_s
        subject = "(no subject)" if subject == ""
        icon = { "Todo" => "[ ]", "Done" => "[X]" }
        cat = item.categories.map{|s| s.to_s}.select{|s| s == "Todo" or s == "Done"}.map{|s| icon[s]}.join(",")
        if cat != ""
          subject = cat + " " + subject
        else
          subject = "    " + subject
        end
        format("%s%-11s %s%s\n",
               format_item_header(context, date, item),
               item.time_range.to_mhc_string.toutf8,
               subject.toutf8,
               append(enclose(item.location)).toutf8
               )
      end

      def format_item_header(context, date, item)
        if context[:number_in_day] == 1
          date.strftime("%Y/%m/%d %a ")
        else
          " " * 15
        end
      end

    end # class Text
  end # module Formatter
end # module Mhc
