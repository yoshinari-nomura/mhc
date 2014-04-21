# -*- coding: utf-8 -*-
module Mhc
  class Calendar
    ALL = 'all'
    attr_reader :datastore
    def initialize(datastore)
      @datastore = datastore
      @logger    = datastore.logger
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

    def search1(date, &predicate_block)
      events = []
      date_to_slots(date).each do |slot|
        date_to_search_keys(date).each do |key|
          next if @db[slot][key].nil?
          @db[slot][key].each do |ev|
            next if !ev.duration.include?(date) or ev.exceptions.map{|range| range.to_a}.flatten.include?(date) or (block_given? and !yield(ev))
            events << ev
          end
        end
      end
      return events.sort{|a,b| a.time_range <=> b.time_range}.uniq
    end

    ################
    #private
    ################
    def register_event(slot, event)
      @db[slot] ||= {}
      event_to_search_keys(event).each do |key|
        @db[slot][key] ||= []
        @db[slot][key] << event
      end
    end

    def update(date_range)
      date_range_to_slots(date_range).each do |slot|
        @db[slot] = {}
        @datastore.entries(slot).each do |path, header|
          if path
            register_event(slot, Mhc::Event.new.parse_file(path))
          else
            register_event(slot, Mhc::Event.new.parse(header))
          end
        end
      end
    end

    def event_to_search_keys(event)
      search_keys = []

      day = event.dates.map{|range| range.to_a.map(&:to_mhc_string) }.flatten
      mon = event.recurrence_condition.cond_mon.map{|m|
        Mhc::PropertyValue::RecurrenceCondition::MON_V2L[m]
      }
      ord = event.recurrence_condition.cond_ord.map{|o|
        Mhc::PropertyValue::RecurrenceCondition::ORD_V2L[o]
      }
      wek = event.recurrence_condition.cond_wek.map{|w|
        Mhc::PropertyValue::RecurrenceCondition::WEK_V2L[w]
      }
      num = event.recurrence_condition.cond_num.map{|n| format("%02d", n)}

      mon = [ALL] if (mon.empty?)
      ord = [ALL] if (ord.empty?)

      # 20140301
      day.each{|yymmdd| search_keys << yymmdd}

      mon.each do |mon|
        # Aug 2nd Thu
        ord.each{|ord| wek.each{|wek| search_keys << mon+ord+wek}}
        # Aug 13
        num.each{|num| search_keys << mon+num}
      end
      return search_keys
    end

    def date_to_search_keys(date)
      mon = date.strftime("%b")
      wek = date.strftime("%a")
      ord = [0, '1st', '2nd', '3rd', '4th', 'Last'].slice(date.week_number_of_month)
      day = date.strftime("%d")
      last = "Last"

      search_keys = [
                     date.strftime("%Y%m%d"),         # 20140328
                     mon+ord+wek,  # Mar 4th Fri
                     mon+ALL+wek,  # Mar Fri (every Friday in March)
                     ALL+ord+wek,  # 4th Fri (every 4th Friday)
                     ALL+ALL+wek,  # Fri     (every Friday)
                     mon+day,      # Mar 28  (March 28 every year)
                     ALL+day       # 28      (28 every month)
                    ]

      if date.last_week_of_month?
        search_keys += [
                        mon+last+wek, # Mar Last Fri
                        ALL+last+wek  # Last Fri
                       ]
      end
      return search_keys
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
