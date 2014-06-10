module Mhc
  class Modifier
    class ParseError < StandardError; end

    attr_accessor :name

    def initialize(name)
      @name = name
    end

    def decorate(event)
      case @name.to_sym

      when :hide_details
        Decorator::HideDetails.new(event)

      when :hide_description
        Decorator::HideDescription.new(event)

      else
        raise Mhc::Modifier::ParseError, "Unknown Decorator #{@name}"
      end
    end

    class Decorator
      require 'forwardable'
      extend Forwardable

      def_delegators :@event,
      :path,
      :alarm,
      :categories,
      :description,
      :body,
      :location,
      :priority,
      :record_id,
      :occurrences,
      :uid,
      :subject,
      :dates,
      :exceptions,
      :time_range,
      :duration,
      :recurrence_condition,
      :recurrence_tag,
      :mission_tag,
      :sequence,
#      :occurrences,
      :dtstart,
      :dtend,
      :rdates,
      :exdates,
      :etag,
      :recurring?,
      :allday?

      def initialize(event)
        @event = event
      end

      def to_ics
        Mhc::Converter::Icalendar.new.to_ics(self)
      end

      def to_icalendar
        Mhc::Converter::Icalendar.new.to_icalendar(self)
      end

      def to_ics_string
        Mhc::Converter::Icalendar.new.to_ics_string(self)
      end

      def occurrences(range:nil)
        Mhc::OccurrenceEnumerator.new(self, dates, exceptions, recurrence_condition, duration, range)
      end

      class HideDetails < Decorator
        def subject
          return Mhc::PropertyValue::Text.new.parse("BUSY")
        end

        def location
          return nil
        end

        def description
          return nil
        end
        alias_method :body, :description

      end # class HideDetails


      class HideDescription < Decorator

        def description
          return nil
        end

        alias_method :body, :description

      end # class HideDescription

    end # class Decorator
  end # class Modifier
end # module Mhc
