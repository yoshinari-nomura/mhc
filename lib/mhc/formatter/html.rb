module Mhc
  class Formatter
    class Html < Base
      TEMPLATE_DIR = File.expand_path("../../templates", __FILE__)

      def initialize(date_range:, options:)
        super(date_range: date_range, options: options)
        @json_formatter = Json.new(date_range: date_range, options: options)
      end

      def <<(occurrence)
        @json_formatter << occurrence
      end

      private

      def format_header(context)
        require "erb"
        template_path = File.expand_path("full-calendar.html.erb", TEMPLATE_DIR)
        @template = ERB.new(File.open(template_path).read, nil, "-")
        return ""
      end

      def format_body(context)
        env = Struct.new(:json_event_array, :tzid)
        mhc = env.new(@json_formatter.to_s, Mhc.default_tzid) # used in ERB template
        @template.result(binding)
      end

    end # class Html
  end # class Formatter
end # module Mhc
