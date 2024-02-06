# -*- coding: utf-8 -*-
module Mhc
  class Calendar

    def initialize(datastore, modifiers = [], &default_scope)
      @datastore     = datastore
      @modifiers     = modifiers || []
      @logger        = @datastore.logger
      @default_scope = default_scope
    end

    def find(uid:)
      if event = @datastore.find_by_uid(uid)
        decorate_event(event)
      end
    end

    def events(date_range = nil, &scope_block)
      occurrences(date_range, &scope_block).map(&:event).uniq
    end

    def tasks(&scope_block)
      @datastore.entries(category: "todo")
    end

    def recurrences(rec, &scope_block)
      @datastore.entries(recurrence: rec)
    end

    def recurrence_tags
      hash = {}
      @datastore.entries.each do |ev|
        tag = ev.recurrence_tag.to_s
        next if tag == ""
        next if hash[tag]

        yield tag
        hash[tag] = ev
      end
    end

    def occurrences(date_range, &scope_block)
      ocs = []
      @datastore.entries(range: date_range).each do |event|
        event = decorate_event(event)
        event.occurrences(range:date_range).each do |oc|
          ocs << oc if in_scope?(oc, &scope_block)
        end
      end
      return ocs.sort
    end

    def add(event)
      @datastore.update(event)
    end

    ################################################################
    ## for sync manager

    def report_etags(uid = nil)
      return find(uid) if uid
      date_range = (Mhc::PropertyValue::Date.today - 90)..
                   (Mhc::PropertyValue::Date.today + 90)
      events(date_range)
    end

    def get_with_etag(uid)
      find(uid: uid)
    end

    def put_if_match(uid, ics_string, expected_etag)
      STDERR.print "Mhc::Calendar#put_if_match(uid:#{uid}, expected_etag:#{expected_etag})..."
      if ev = find(uid: uid) and ev.etag != expected_etag
        STDERR.print "failed: etag not match #{ev.etag} != #{expected_etag}\n"
        return nil
      end
      if expected_etag and (not ev)
        STDERR.print "failed: etag not match #{expected_etag} != nil\n"
      end
      begin
        ev = Mhc::Event.new_from_ics(ics_string)
        @datastore.update(ev)
        STDERR.print "succeeded #{ev.etag}\n"
        return true
      rescue Exception => e
        STDERR.print "failed: #{e.to_s}\n"
        STDERR.print "#{e.backtrace.first}\n" if $MHC_DEBUG
        STDERR.print "#{ics_string}\n" if $MHC_DEBUG
        return nil
      end
    end

    def delete_if_match(uid, expected_etag)
      STDERR.print "Mhc::Calendar#delete_if_match(uid:#{uid}, expected_etag:#{expected_etag})..."
      unless ev = find(uid: uid)
        STDERR.print "failed: uid #{uid} not found\n"
        return nil
      end
      if expected_etag && ev.etag != expected_etag
        STDERR.print "failed: etag not match #{ev.etag} != #{expected_etag}\n"
        return nil
      end
      begin
        @datastore.delete(ev)
        STDERR.print "succeeded: #{ev.etag}\n"
        return ev
      rescue Exception => e
        STDERR.print "failed: #{e.to_s}\n"
        return nil
      end
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
