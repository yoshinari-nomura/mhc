# -*- coding: utf-8 -*-

module Mhc
  class FormatterNameError < StandardError; end

  class Formatter
    def self.build(formatter:, date_range: date_range, **options)
      case formatter.to_sym
      when :text
        Text.new(date_range: date_range, options:options)
      when :mail
        Mail.new(date_range: date_range, options:options)
      when :orgtable
        OrgTable.new(date_range: date_range, options:options)
      when :emacs
        # SymbolicExpression.new(options)
        Emacs.new(date_range: date_range, options:options)
      when :icalendar
        ICalendar.new(date_range: date_range, options:options)
      when :howm
        Howm.new(date_range: date_range, options:options)
      else
        raise FormatterNameError.new("Unknown format: #{formatter} (#{formatter.class})")
      end
    end

    class Base
      def initialize(date_range: date_range, options:nil)
        @items = {}
        @options = options
        @date_range = date_range
      end

      def <<(occurrence)
        (occurrence.first..occurrence.last).each do |date|
          @items[date] ||= []
          @items[date] << occurrence
        end
      end

      def to_s
        context = {:items => @items}.merge(@options)
        return format_header(context) + format_body(context) + format_footer(context)
      end

      ################################################################
      private

      def format_header(context)
        ""
      end

      def format_footer(context)
        ""
      end

      def format_body(context)
        context[:number] = 0

        # XXX for padding empty days
        @date_range.each do |date|
          @items[date] ||= []
        end

        @items.keys.sort.map{|date| format_day(context, date, @items[date]) }.join
      end

      def format_day(context, date, items)
        string = format_day_header(context, date)

        items.each_with_index do |schedule, count|
          context[:number] += 1
          context[:number_in_day] = count + 1
          string += format_item(context, date, schedule)
        end

        return string + format_day_footer(context, date)
      end

      def format_day_header(context, date)
        ""
      end

      def format_day_footer(context, date)
        ""
      end

      def format_item(context, date, item)
        format("%s%-11s %s%s\n",
               format_item_header(context, date, item),
               item.time_range.to_mhc_string.toutf8,
               item.subject.to_s.toutf8,
               append(enclose(item.location)).toutf8
               )
      end

      def format_item_header(context, date, item)
        if context[:number_in_day] == 1
          date.strftime("%m/%d %a ")
        else
          " " * 10
        end
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
    end

    class Text < Base
    end # class Text

    class Mail < Base
      private

      def format_header(context)
        mail_address = context[:mailto].to_s
        subject = format("MHC schedule report (%s--%s)", *context[:items].keys.minmax)
        header =  "To: #{mail_address}\n"
        header += "From: #{append(mail_address, "secretary-of-")}\n"
        header += "Subject: #{subject}\n"
        header += "Content-Type: Text/Plain; charset=utf-8\n"
        header += "Content-Transfer-Encoding: 8bit\n"
        header += "\n"
        header += format("* mhc %s--%s\n", *context[:items].keys.minmax)
      end
    end # class Mail

    class SymbolicExpression < Base
      private

      def format_header(context)
        "("
      end

      def format_footer(context)
        ")\n"
      end

      def format_day_header(context, date)
        date.strftime("((%2m %2d %Y) . (")
      end

      def format_item(context, date, item)
        '"' +
          format("%s%s%s",
                 prepend(item.time_range.to_s).toutf8,
                 item.subject.to_s.toutf8,
                 append(enclose(item.location)).toutf8).gsub(/[\"\\]/, '\\\\\&') +
          '" '
      end

      def format_day_footer(context, date)
        ")) "
      end
    end

    class Emacs < SymbolicExpression
      def format_day_header(context, date)
        format("(%d . [%d %d %d %d nil (", date.absolute_from_epoch, date.year, date.month, date.day, date.wday)
      end

      def format_item(context, date, item)
        # [ RECORD CONDITION SUBJECT LOCATION (TIMEB . TIMEE) ALARM CATEGORIES PRIORITY REGION RECURRENCE-TAG]
        format("[(%s . [%s nil nil]) nil %s %s (%s . %s) %s (%s) nil nil %s]",
               elisp_string(item.path.to_s),
               elisp_string(item.uid.to_s),
               elisp_string(item.subject),
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
        '"' + string.to_s.toutf8.gsub(/[\"\\]/, '\\\\\&') + '"'
      end
    end

    class ICalendar < Base
      def initialize(options = nil)
        super(options)
        @ical = RiCal.Calendar
        @ical.default_tzid = "Asia/Tokyo"
        @events = {}
      end

      private

      def format_item(context, date, item)
        return "" if @events[item.event] == true
        @ical.events << item.event.to_icalendar
        @events[item.event] = true
        return ""
      end

      def format_footer(context)
        @ical.to_s
      end
    end

    class OrgTable < Base
      private

      def format_header(context)
        format("* mhc %s--%s\n", *context[:items].keys.minmax)
      end

      def format_item(context, date, item)
        # | No | Mission | Recurrence | Subject | Path | Date |
        format("  | %4d | %s | %s | %s | %s | [%04d-%02d-%02d%s] |\n",
               context[:number],
               item.mission_tag.to_s.toutf8,
               item.recurrence_tag.to_s.toutf8,
               item.subject.to_s.toutf8,
               item.path.to_s,
               date.year, date.month, date.mday,
               append(item.time_range.to_s))
      end
    end # class OrgTable

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

  end # module Formatter
end # module Mhc
