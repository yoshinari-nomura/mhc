module Mhc
  class Formatter
    class Icalendar < Base
      private

      def format_body(context)
        ical = RiCal.Calendar
        ical.prodid = Mhc::PRODID
        @events.each do |event|
          ical.events << event.to_icalendar
        end
        return ical.to_s
      end

    end # class Icalendar
  end # class Formatter
end # module Mhc
