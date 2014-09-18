# -*- coding: utf-8 -*-
module Mhc
  class Calendar

    def initialize(datastore, modifiers = [], &default_scope)
      @datastore     = datastore
      @modifiers     = modifiers || []
      @logger        = @datastore.logger
      @default_scope = default_scope
    end

    def find(uid: uid)
      if event = @datastore.find_by_uid(uid)
        decorate_event(event)
      end
    end

    def events(date_range = nil, &scope_block)
      occurrences(date_range, &scope_block).map(&:event).uniq
    end

    def occurrences(date_range, &scope_block)
      ocs = []
      @datastore.entries(date_range).each do |event|
        event = decorate_event(event)
        event.occurrences(range:date_range).each do |oc|
          ocs << oc if in_scope?(oc, &scope_block)
        end
      end
      return ocs.sort
    end

    def report_etags(uid = nil)
      return find(uid) if uid
      date_range = (Mhc::PropertyValue::Date.today - 90)..
                   (Mhc::PropertyValue::Date.today + 90)
      events(date_range)
    end

    def get_with_etag(uid)
      find(uid: uid)
    end

    ################################################################
    private
    ################################################################

    def decorate_event(event)
      @modifiers.each do |deco|
        event = deco.decorate(event)
      end
      return event
    end

    def in_scope?(oc, &scope_block)
      (!@default_scope || @default_scope.call(oc)) &&
        (!scope_block || scope_block.call(oc))
    end

  end # class Calendar
end # module Mhc
