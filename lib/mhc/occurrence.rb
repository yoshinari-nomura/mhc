require 'forwardable'

module Mhc
  class Occurrence
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
    :uid,
    :subject,
    :time_range,
    :recurrence_tag,
    :mission_tag,
    :allday?

    attr_reader :event, :date

    def initialize(event, date)
      @event, @date = event, date
    end

    def dtstart
      if allday?
        @date
      else
        time_range.first.to_datetime(@date)
      end
    end

    def dtend
      if allday?
        @date + 1
      else
        time_range.last.to_datetime(@date)
      end
    end
  end # class Event
end # module Mhc
