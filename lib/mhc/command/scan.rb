module Mhc
  module Command
    class Scan
      Encoding.default_external = "UTF-8"

      def initialize(calendar, range_string, format: :text, search: nil, **options)
        formatter = Mhc::Formatter.build(formatter: format, **options)
        date_range = Mhc::PropertyValue::Date.parse_range(range_string)
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

        calendar.scan(date_range, &search_proc).each do |date, items|
          formatter << [date, items]
        end
        print formatter.to_s
      end
    end # class Scan
  end # module Command
end # module Mhc
