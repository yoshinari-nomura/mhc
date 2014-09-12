# -*- coding: utf-8 -*-
module Mhc
  class Calendar

    def initialize(datastore, modifiers = [], &default_scope)
      @db = {}
      @datastore     = datastore
      @modifiers     = modifiers || []
      @logger        = @datastore.logger
      @default_scope = default_scope
    end

    def find(uid: uid)
      if data = @datastore.find_by_uid(uid)
        build_event(data:data)
      end
    end

    def entries
      Enumerator.new do |yielder|
        @datastore.entries.each do |path, header|
          if path
            yielder << Mhc::Event.parse_file(path)
          else
            yielder << Mhc::Event.parse(header)
          end
        end
      end
    end

    def scan(date_range, &scope_block)
      update(date_range)
      date_range.map {|date| [date, search1(date, &scope_block)]}
    end

    def occurrences(date_range, &scope_block)
      ocs = []
      date_range_to_slots(date_range).each do |slot|
        @datastore.entries(slot).each do |path, header|
          if path
            event = build_event(path:path)
          else
            event = build_event(data:header)
          end
          event.occurrences(range:date_range).each do |oc|
            ocs << oc unless oc.last < date_range.first or date_range.last < oc.first
          end
        end
      end
      ocs.select!{|oc| in_scope?(oc, &scope_block)}
      return ocs.sort
    end

    def report_etags(uid = nil)
      @events = {}
      scan((Date.today - 90)..(Date.today + 90)).each do |date, occs|
        occs.each do |occ|
          next if @events[occ.uid]
          @events[occ.uid] = occ.event
        end
      end
      return @events.values
    end

    def get_with_etag(uid)
      @events[uid]
    end

    ################################################################
    private
    ################################################################

    def build_event(path:nil, data:data)
      if path
        event = Mhc::Event.parse_file(path)
      else
        event = Mhc::Event.parse(data)
      end
      decorate_event(event)
    end

    def decorate_event(event)
      @modifiers.each do |deco|
        event = deco.decorate(event)
      end
      return event
    end

    def search1(date, &scope_block)
      occurrences = []
      date_to_slots(date).each do |slot|
        occurrences += @db[slot][date] || []
      end
      occurrences.select!{|oc| in_scope?(oc, &scope_block)}
      occurrences.sort{|a,b| a.time_range <=> b.time_range}.uniq
    end

    def in_scope?(oc, &scope_block)
      (!@default_scope || @default_scope.call(oc)) &&
        (!scope_block || scope_block.call(oc))
    end

    def register_event(slot, event, date_range)
      event.occurrences(range:date_range).each do |oc|
        @db[slot][oc.date] ||= []
        @db[slot][oc.date] << oc
      end
    end

    def update(date_range)
      date_range_to_slots(date_range).each do |slot|
        @db[slot] = {}
        @datastore.entries(slot).each do |path, header|
          if path
            register_event(slot, build_event(path:path), date_range)
          else
            register_event(slot, build_event(data:header), date_range)
          end
        end
      end
    end

    # determine slots from which the events in date_rage can be picked
    def date_range_to_slots(date_range)
      min = date_range.min
      max = date_range.max
      max = max.class.new(max.year, max.month, -1)
      slots = []

      while min <= max
        slots += date_to_slots(min)
        min = min.next_month
      end
      return slots.sort.uniq
    end

    # determine slots from which the events in date can be picked
    def date_to_slots(date)
      return ["inbox", "intersect", "presets", date_to_slot(date)]
    end

    # determine slots from which the events in date can be picked
    def date_to_slot(date)
      return date.strftime("%Y/%m")
    end

    # determine a slot to which the new event should be registered
    def event_to_slot(event)
      if event.occur_inter_month?
        return "intersect"
      else
        return event.first_ocurrence.strftime("%Y/%m")
      end
    end
  end # class Calendar
end # module Mhc
