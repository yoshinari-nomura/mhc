# -*- coding: utf-8 -*-
module Mhc
  class Calendar

    attr_reader :datastore

    def initialize(datastore = nil)
      @datastore = datastore || Mhc::DataStore.new
      @logger    = @datastore.logger
      @db = {}
    end

    def delete_event(event, add_log = true)
      @datastore.delete(event.uid)
      if add_log
        @logger << Mhc::LogEntry.new('D', Time.now, event.uid, @datastore.path(event.uid), event.subject)
      end
      event.delete
      return self
    end

    def add_event(event, add_log = true)
      @datastore.add(event.uid, event.dump, event_to_slot(event))
      if add_log
        @logger <<  Mhc::LogEntry.new('M', Time.now, event.uid, @datastore.path(event.uid), event.subject)
      end
      return self
    end

    def scan(date_range, &predicate_block)
      update(date_range)
      date_range.map {|date| [date, search1(date, &predicate_block)]}
    end

    def occurrences(date_range, &predicate_block)
      ocs = []
      date_range_to_slots(date_range).each do |slot|
        @datastore.entries(slot).each do |path, header|
          if path
            event = Mhc::Event.parse_file(path)
          else
            event = Mhc::Event.parse(header)
          end
          event.occurrences(range:date_range).map{|oc| ocs << oc if date_range.include?(oc.first) or date_range.include?(oc.last)}
        end
      end
      ocs.select!{|oc| yield oc } if block_given?
      return ocs.sort
    end

    def report_etags(uid = nil)
      @events = {}
      scan((Date.today - 14)..(Date.today + 30)).each do |date, occs|
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

    def search1(date, &predicate_block)
      occurrences = []
      date_to_slots(date).each do |slot|
        occurrences += @db[slot][date] || []
      end
      occurrences.select!{|oc| yield oc } if block_given?
      occurrences.sort{|a,b| a.time_range <=> b.time_range}.uniq
    end

    # for debug
    def dump_db
      @db.keys.each do |slot|
        puts "* #{slot}"
        @db[slot].keys.sort.each do |date|
          puts "** #{date}"
          @db[slot][date].each do |oc|
            puts "*** #{oc.subject}"
          end
        end
      end
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
            register_event(slot, Mhc::Event.new.parse_file(path), date_range)
          else
            register_event(slot, Mhc::Event.new.parse(header), date_range)
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
      return ["intersect", "anniversaries", date_to_slot(date)]
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
