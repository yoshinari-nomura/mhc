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
  # * X-SC-Recurrence-Tag:
  # * X-SC-Mission-Tag:
  # * X-SC-Cond:
  # * X-SC-Duration:
  # * X-SC-Alarm:
  # * X-SC-Record-Id:
  # * X-SC-Sequence:
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

    ## record-id
    def record_id
      return @record_id ||= Mhc::PropertyValue::Text.new
    end

    def record_id=(string)
      return @record_id = record_id.parse(string)
    end

    def uid
      record_id.to_s
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
        Mhc::PropertyValue::List.new(Mhc::PropertyValue::Range.new(Mhc::PropertyValue::Date.new))
    end

    def dates=(string)
      string = string.split.select {|s| /^!/ !~ s}.join(" ")
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
        Mhc::PropertyValue::List.new(Mhc::PropertyValue::Range.new(Mhc::PropertyValue::Date.new, "!"))
    end

    def exceptions=(string)
      string = string.split.select {|s| /^!/ =~ s}.map{|s| s[1..-1]}.join(" ")
      return @exceptions = exceptions.parse(string)
    end

    ## time
    def time_range
      return @time_range ||=
        Mhc::PropertyValue::Range.new(Mhc::PropertyValue::Time)
    end

    def time_range=(string)
      @time_range = time_range.parse(string)
      return @time_range
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

    ## mission-tag
    def mission_tag
      return @mission_tag ||= Mhc::PropertyValue::Text.new
    end

    def mission_tag=(string)
      return @mission_tag = mission_tag.parse(string)
    end

    ## sequence
    def sequence
      return @sequence ||= Mhc::PropertyValue::Integer.new.parse("0")
    end

    def sequence=(string)
      return @sequence = sequence.parse(string.to_s)
    end

    def occurrences(range:nil)
      Mhc::OccurrenceEnumerator.new(self, dates, exceptions, recurrence_condition, duration, range)
    end

    def etag
      return "#{uid.to_s}-#{sequence.to_s}"
    end

    def recurring?
      not recurrence_condition.empty?
    end

    def allday?
      time_range.blank?
    end

    def range
      min0, max0 = Mhc::PropertyValue::Date.parse("19000101"),
                   Mhc::PropertyValue::Date.parse("99991231")

      if recurring?
        min, max = min0, max0
      else
        min, max = dates.min, dates.max
        min = min.first if min.respond_to?(:first)
        max = max.last  if max.respond_to?(:last)
      end
      min = duration.first if duration.first && duration.first > min
      max = duration.last  if duration.last  && duration.last  < max

      return min..max if min <= max
      return min0..max0
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
        "X-SC-Mission-Tag: #{mission_tag.to_mhc_string}\n"   +
        "X-SC-Recurrence-Tag: #{recurrence_tag.to_mhc_string}\n"       +
        "X-SC-Cond: #{recurrence_condition.to_mhc_string}\n" +
        "X-SC-Duration: #{duration.to_mhc_string}\n"         +
        "X-SC-Alarm: #{alarm.to_mhc_string}\n"               +
        "X-SC-Record-Id: #{record_id.to_mhc_string}\n"       +
        "X-SC-Sequence: #{sequence.to_mhc_string}\n"
    end

    alias_method :to_mhc_string, :dump

    ################################################################
    ### converter

    def to_ics
      Mhc::Converter::Icalendar.new.to_ics(self)
    end

    def to_icalendar
      Mhc::Converter::Icalendar.new.to_icalendar(self)
    end

    def to_ics_string
      Mhc::Converter::Icalendar.new.to_ics_string(self)
    end

    ################################################################
    private

    def lazy?
      return !@path.nil?
    end

    def clear
      @alarm, @categories, @description, @location = [nil]*4
      @record_id, @subject = [nil]*2
      @dates, @exceptions, @time_range, @duration, @cond, @oc = [nil]*6
      @non_xsc_header, @path = [nil]*2
      return self
    end

    def parse_header_full(string)
      xsc, @non_xsc_header = separate_header(string)
      parse_xsc_header(xsc)
      return self
    end

    def parse_header(string)
      hash = {}
      string.scan(/^x-sc-([^:]++):[ \t]*([^\n]*(?:\n[ \t]+[^\n]*)*)/i) do |key, val|
        hash[key.downcase] = val.gsub("\n", " ").strip
      end
      parse_xsc_header(hash)
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
        when "mission-tag"  ; self.mission_tag = val
        when "recurrence-tag"  ; self.recurrence_tag = val
        when "cond"      ; self.recurrence_condition  = val
        when "alarm"     ; self.alarm      = val
        when "record-id" ; self.record_id  = val
        when "sequence"  ; self.sequence   = val
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
