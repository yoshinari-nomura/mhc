module Mhc
  module Command
    class Scan
      Encoding.default_external = "UTF-8"

      def initialize(calendar, range_string, format: :text, search: nil, **options)
        date_range = Mhc::PropertyValue::Date.parse_range(range_string)
        formatter = Mhc::Formatter.build(formatter: format, date_range: date_range, **options)
        format_range(calendar, formatter, date_range, search: search, **options)
      end

      def format_range(calendar, formatter, date_range, search: nil, **options)
        if search
          begin
            search_proc = Mhc::Query.new(search).to_proc
          rescue Mhc::Query::ParseError => e
            STDERR.print "Error: " + e.message + " in search option.\n"
            exit 1
          end
        end

        calendar.occurrences(date_range, &search_proc).each do |oc|
          formatter << oc
        end
        print formatter.to_s
      end
    end # class Scan
  end # module Command
end # module Mhc
