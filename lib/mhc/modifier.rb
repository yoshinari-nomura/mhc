module Mhc
  class Modifier
    class ParseError < StandardError; end

    attr_accessor :name

    def initialize(name)
      @name = name
    end

    def decorate(event)
      if deco = Decorator.find_subclass(@name.to_sym)
        deco.new(event)
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

      def self.find_subclass(snake_name)
        @subclasses ||= {}

        if c = @subclasses[snake_name]
          return c
        end

        class_name = snake_name.to_s.capitalize.gsub(/_([a-z\d]+)/){ $1.capitalize }.to_sym
        return nil unless const_defined?(class_name)

        const = const_get(class_name)
        if const.class == Class and const.superclass == self
          return @subclasses[snake_name] = const
        end
        return nil
      end

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


      class HideLocation < Decorator
        def location
          return nil
        end
      end # class HideLocation


      class HideTimeRange <  Decorator
        def allday?
          return true
        end

        def time_range
          # create empty time_range
          Mhc::PropertyValue::Range.new(Mhc::PropertyValue::Time)
        end
      end # class HideTimeRange
    end # class Decorator
  end # class Modifier
end # module Mhc
