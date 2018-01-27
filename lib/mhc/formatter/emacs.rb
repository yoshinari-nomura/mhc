module Mhc
  class Formatter
    class Emacs < SymbolicExpression
      private

      def prepare(context)
        expand_multiple_days_occurrences
      end

      def format_header(context);  "(";   end
      def format_footer(context);  ")\n"; end

      def format_day_header(context, date, is_holiday)
        # (DAYS_FROM_EPOC . [year month day wday holiday-p (
        format("(%d . [%d %d %d %d #{is_holiday ? 't' : 'nil'} (",
               date.absolute_from_epoch, date.year, date.month, date.day, date.wday)
      end

      def format_item(context, date, item)
        subject = item.subject.to_s
        subject = "(no subject)" if subject == ""

        # [ RECORD CONDITION SUBJECT LOCATION (TIMEB . TIMEE) ALARM
        # CATEGORIES PRIORITY REGION RECURRENCE-TAG]
        format("[(%s . [%s nil nil]) nil %s %s (%s . %s) %s (%s) nil nil %s]",
               elisp_string(item.path.to_s),
               elisp_string(item.uid.to_s),
               elisp_string(subject),
               elisp_string(item.location),
               (item.time_range.first ? (item.time_range.first.to_i / 60) : "nil"),
               (item.time_range.last  ? (item.time_range.last.to_i  / 60) : "nil"),
               elisp_string(item.alarm.to_s),
               item.categories.map{|c| elisp_string(c.to_s.downcase)}.join(" "),
               elisp_string(item.recurrence_tag))
      end

      def format_day_footer(context, date)
        ")]) "
      end

      def elisp_string(string)
        '"' + string.to_s.toutf8.gsub(/[\"\\]/, '\\\\\&').gsub("\n", "\\n") + '"'
      end

    end # class Emacs
  end # class Formatter
end # module Mhc
