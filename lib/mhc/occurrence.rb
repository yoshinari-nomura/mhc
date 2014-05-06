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
  end # class Event
end # module Mhc
