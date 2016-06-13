module Mhc
  class Formatter
    class Json < Base
      require "json"

      def format_body(context)
        events = []
        @occurrences.each do |oc|
          class_name = []
          class_name += oc.categories.map{|c| "mhc-category-#{c.to_s.downcase}"}
          class_name << (oc.allday? ? "mhc-allday" : "mhc-time-range")

          events << {
            id:    oc.record_id,
            allDay: oc.allday?,
            title: oc.subject,
            start: oc.dtstart.iso8601,
            end:   oc.dtend.iso8601,
            className: class_name
          }
        end
        return events.to_json
      end

    end # class Json
  end # class Formatter
end # module Mhc
