# -*- coding: utf-8 -*-

### event.rb
##
## Author:  Yoshinari Nomura <nom@quickhack.net>
##
## Created: 1999/07/16
## Revised: $Date: 2008-10-08 03:22:37 $
##

module Mhc
  # Mhc::Event defines a simple representation of calendar events.
  # It looks like a RFC822 message with some X- headers to represent event properties:
  # * X-SC-Subject:
  # * X-SC-Location:
  # * X-SC-Day:
  # * X-SC-Time:
  # * X-SC-Category:
  # * X-SC-Priority:
  # * X-SC-Cond:
  # * X-SC-Duration:
  # * X-SC-Alarm:
  # * X-SC-Record-Id:
  #
  class Event
    ################################################################
    ## initializers

    def initialize
      clear
    end

    def self.parse(string)
      return new.parse(string)
    end

    def self.parse_file(path, lazy = true)
      return new.parse_file(path, lazy)
    end

    def parse_file(path, lazy = true)
      clear
      header, body = nil, nil

      File.open(path, "r") do |file|
        header = file.gets("\n\n")
        body   = file.gets(nil) unless lazy
      end

      @path = path if lazy
      parse_header(header)
      self.description = body if body
      return self
    end

    def parse(string)
      clear
      header, body = string.scrub.split(/\n\n/, 2)

      parse_header(header)
      self.description = body
      return self
    end

    def path
      return @path
    end
    ################################################################
    ## access methods to each property.

    ## alarm
    def alarm
      return @alarm ||= Mhc::PropertyValue::Period.new
    end

    def alarm=(string)
      return @alarm = alarm.parse(string)
    end

    ## category
    def categories
      return @categories ||=
        Mhc::PropertyValue::List.new(Mhc::PropertyValue::Text)
    end

    def categories=(string)
      return @categories = categories.parse(string)
    end

    ## description
    def description
      unless @description
        @description = Mhc::PropertyValue::Text.new

        if lazy? && File.file?(@path)
          File.open(@path, "r") do |file|
            file.gets("\n\n") # discard header.
            @description.parse(file.gets(nil))
          end
        end
      end
      return @description
    end
    alias_method :body, :description

    def description=(string)
      return @description = description.parse(string)
    end

    ## location
    def location
      return @location ||= Mhc::PropertyValue::Text.new
    end

    def location=(string)
      return @location = location.parse(string)
    end

    ## priority
    def priority
      return @priority ||= Mhc::PropertyValue::Integer.new
    end

    def priority=(string)
      return @priority = priority.parse(string)
    end

    ## record-id
    def record_id
      return @record_id ||= Mhc::PropertyValue::Text.new
    end
    alias_method :uid, :record_id

    def record_id=(string)
      return @record_id = record_id.parse(string)
    end

    ## subject
    def subject
      return @subject ||= Mhc::PropertyValue::Text.new
    end

    def subject=(string)
      return @subject = subject.parse(string)
    end

    ## date list is a list of date range
    def dates
      return @dates ||=
        # Mhc::PropertyValue::List.new(Mhc::PropertyValue::Range.new(Mhc::PropertyValue::Date))
        Mhc::PropertyValue::List.new(Mhc::PropertyValue::Date)
    end

    def dates=(string)
      return @dates = dates.parse(string)
    end

    def obsolete_dates=(string)
      # STDERR.print "Obsolete X-SC-Date: header.\n"
      if /(\d+)\s+([A-Z][a-z][a-z])\s+(\d+)\s+(\d\d:\d\d)/ =~ string
        dd, mm, yy, hhmm = $1.to_i, $2, $3.to_i + 1900, $4
        mm = ("JanFebMarAprMayJunJulAugSepOctNovDec".index(mm)) / 3 + 1
        @dates = dates.parse("%04d%02d%02d" % [yy, mm, dd])
        if hhmm and hhmm != '00:00'
          @time_range = time_range.parse(hhmm)
        end
      end
    end

    def exceptions
      return @exceptions ||=
        Mhc::PropertyValue::List.new(Mhc::PropertyValue::Date, "!")
        # Mhc::PropertyValue::List.new(
        #  Mhc::PropertyValue::Range.new(
        #    Mhc::PropertyValue::Date, "!"))
    end

    def exceptions=(string)
      return @exceptions = exceptions.parse(string)
    end

    ## time
    def time_range
      return @time_range ||=
        Mhc::PropertyValue::Range.new(Mhc::PropertyValue::Time)
    end

    def time_range=(string)
      return @time_range =  time_range.parse(string)
    end

    ## duration
    def duration
      return @duration ||=
        Mhc::PropertyValue::Range.new(Mhc::PropertyValue::Date)
    end

    def duration=(string)
      return @duration = duration.parse(string)
    end

    ## recurrence condition
    def recurrence_condition
      return @cond ||= Mhc::PropertyValue::RecurrenceCondition.new
    end

    def recurrence_condition=(string)
      return @cond = recurrence_condition.parse(string)
    end

    ## recurrence-tag
    def recurrence_tag
      return @recurrence_tag ||= Mhc::PropertyValue::Text.new
    end

    def recurrence_tag=(string)
      return @recurrence_tag = recurrence_tag.parse(string)
    end

    ################################################################
    ## occurrence_caluculator

    def occurrence_caluculator
      @oc ||= Mhc::OccurrenceCalculator.new(dates,
                                            time_range,
                                            exceptions,
                                            recurrence_condition,
                                            duration)
      return @oc
    end

    def dtstart
      @oc.dtstart
    end

    def dtend
      @oc.dtend
    end

    ################################################################
    ## predicates

    def todo?
      return categories.include?("Todo")
    end

    ################################################################
    ### dump

    def dump
      non_xsc_header = @non_xsc_header.to_s.sub(/\n+\z/, "")
      non_xsc_header += "\n" if non_xsc_header != ""

      body = description.to_mhc_string
      body += "\n" if body != "" && body !~ /\n\z/

      return dump_header + non_xsc_header + "\n" + body
    end

    def dump_header
      return "X-SC-Subject: #{subject.to_mhc_string}\n"      +
        "X-SC-Location: #{location.to_mhc_string}\n"         +
        "X-SC-Day: " + "#{dates.to_mhc_string} #{exceptions.to_mhc_string}".strip + "\n" +
        "X-SC-Time: #{time_range.to_mhc_string}\n"           +
        "X-SC-Category: #{categories.to_mhc_string}\n"       +
        "X-SC-Recurrence-Tag: #{recurrence_tag.to_mhc_string}\n"       +
        "X-SC-Priority: #{priority.to_mhc_string}\n"         +
        "X-SC-Cond: #{recurrence_condition.to_mhc_string}\n" +
        "X-SC-Duration: #{duration.to_mhc_string}\n"         +
        "X-SC-Alarm: #{alarm.to_mhc_string}\n"               +
        "X-SC-Record-Id: #{record_id.to_mhc_string}\n"
    end

    alias_method :to_mhc_string, :dump

    ################################################################
    ### converter

    def to_ics_string
      result = self.to_ics_calendar.to_s
      STDERR.print "Event#to_ics_string #{result}\n"
      return result
    end

    def to_ics_event
      ev = self
      event = RiCal.Event do |iev|
        if ev.allday?
          dtstart = ev.start_time.to_date
          dtend   = ev.end_time.to_date + 1
        else
          dtstart = ev.start_time
          dtend   = ev.end_time
        end

        description = ''
        unless ev.recurrence_id.blank?
          if /^(#![a-zA-Z ]+)/ =~ ev.description.to_s
            description = ev.description.to_s.sub($1, '#!' + ev.recurrence_id)
          else
            description = '#!' + ev.recurrence_id.to_s + "\n" + ev.description.to_s
          end
        end

        iev.created       = ev.created_at
        iev.last_modified = ev.updated_at
        iev.uid           = ev.uid
        iev.dtstart       = dtstart
        iev.dtend         = dtend
        iev.summary       = ev.name.to_s
        iev.description   = description
        iev.sequence      = (ev.sequence || 0)
        iev.dtstamp       = Time.now
      end
      return event
    end

    ################################################################
    private

    def lazy?
      return !@path.nil?
    end

    def clear
      @alarm, @categories, @description, @location = [nil]*4
      @priority, @record_id, @subject = [nil]*3
      @dates, @exceptions, @time_range, @duration, @cond, @oc = [nil]*6
      @non_xsc_header, @path = [nil]*2
      return self
    end

    def parse_header(string)
      xsc, @non_xsc_header = separate_header(string)
      parse_xsc_header(xsc)
      return self
    end

    def parse_xsc_header(hash)
      hash.each do |key, val|
        case key
        when "day"       ; self.dates      = val ; self.exceptions = val
        when "date"      ; self.obsolete_dates = val
        when "subject"   ; self.subject    = val
        when "location"  ; self.location   = val
        when "time"      ; self.time_range = val
        when "duration"  ; self.duration   = val
        when "category"  ; self.categories = val
        when "recurrence-tag"  ; self.recurrence_tag = val
        when "cond"      ; self.recurrence_condition  = val
        when "alarm"     ; self.alarm      = val
        when "record-id" ; self.record_id  = val
        when "priority"  ; self.priority   = val
        else
          # raise NotImplementedError, "X-SC-#{key.capitalize}"
          # STDERR.print "Obsolete: X-SC-#{key.capitalize}\n"
        end
      end
      return self
    end

    ## return: X-SC-* headers as a hash and
    ##         non-X-SC-* headers as one string.
    def separate_header(header)
      xsc, non_xsc, xsc_key = {}, "", nil

      header.split("\n").each do |line|
        if line =~ /^X-SC-([^:]+):(.*)/i
          xsc_key = $1.downcase
          xsc[xsc_key] = $2.to_s.strip

        elsif line =~ /^\s/ && xsc_key
          xsc[xsc_key] += " " + line

        else
          xsc_key = nil
          non_xsc += line + "\n"
        end
      end
      return [xsc, non_xsc]
    end

  end # class Event
end # module Mhc
