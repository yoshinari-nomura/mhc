module Mhc
  class Converter
    class Emacs
      # return cfw:event structure
      #
      #  (defstruct cfw:event
      #    title       ; event title [string]
      #    start-date  ; start date of the event [cfw:date]
      #    start-time  ; start time of the event (optional)
      #    end-date    ; end date of the event [cfw:date] (optional)
      #    end-time    ; end of the event (optional)
      #    description ; event description [string] (optional)
      #    location    ; location [strting] (optional)
      #    source      ; [internal] source of the event
      #  )
      def to_calfw(ev)
        hash = {
          :title       => ev.subject.to_s,
          :start_date  => "",
          :start_time  => "",
          :end_date    => "",
          :end_time    => "",
          :description => "",
          :location    => "",
          :source      => ""
        }
        to_emacs_plist(hash)
      end

      def to_emacs(obj)
        case obj
        when Array
          to_emacs_list(obj)
        when Hash
          to_emacs_plist(obj)
        else
          to_emacs_string(obj)
        end
      end

      def to_emacs_symbol(obj)
        ":" + obj.to_s.downcase.gsub('_', '-')
      end

      def to_emacs_string(str)
        # 1. quote " and \
        # 2. surround by "
        '"' + str.to_s.toutf8.gsub(/[\"\\]/, '\\\\\&') + '"'
      end

      def to_emacs_plist(hash)
        wrap(hash.map{|key,val| "#{to_emacs_symbol(key)} #{to_emacs(val)}"}.join(" "))
      end

      def to_emacs_list(array)
        wrap(array.map{|val| to_emacs(val)}.join(" "))
      end

      private
      def wrap(obj)
        "(" + obj.to_s + ")"
      end
    end # class Emacs

    class Icalendar

      def to_ics(event)
        return to_icalendar(event).to_s
      end

      def to_ics_string(event)
        ical = RiCal.Calendar
        ical.prodid = Mhc::PRODID
        ical.events << to_icalendar(event)
        return ical.to_s
      end

      def to_icalendar(event)
        icalendar = RiCal.Event do |iev|
          iev.rrule         = event.recurrence_condition.to_ics(dtstart(event), event.duration.last) if event.recurring?
          iev.exdates       = [exdates(event)] if exdates(event)
          iev.rdates        = [rdates(event)]  if rdates(event)
          iev.created       = created(event).utc.strftime("%Y%m%dT%H%M%SZ")
          iev.categories    = event.categories.to_a unless event.categories.empty?
          iev.location      = event.location.to_s unless event.location.to_s.empty?
          iev.last_modified = last_modified(event).utc.strftime("%Y%m%dT%H%M%SZ")
          iev.uid           = event.uid.to_s
          iev.dtstart       = dtstart(event)
          iev.dtend         = dtend(event)
          iev.summary       = event.subject.to_s
          iev.description   = event.description.to_s
          iev.sequence      = (event.sequence.to_i || 0)
          iev.dtstamp       = ::Time.now.utc.strftime("%Y%m%dT%H%M%SZ")
        end
        return icalendar
      end

      ################################################################
      private

      # DTSTART (RFC5445:iCalendar) has these two meanings:
      # 1) first ocurrence date of recurrence events
      # 2) start date of a single-shot event
      #
      # In MHC, DTSTART should be calculated as:
      #
      # if a MHC article has a Cond: field,
      #   + DTSTART is calculated from Duration: and Cond: field.
      #   + Additional Day: field is recognized as RDATE.
      # else
      #   + DTSTART is calculated from a first entry of Days: field.
      #   + Remains in Day: field is recognized as RDATE.
      # end
      #
      def dtstart(event)
        if event.recurring?
          Mhc::OccurrenceEnumerator.new(event, empty_dates, empty_dates, event.recurrence_condition, event.duration).first.dtstart
        else
          Mhc::OccurrenceEnumerator.new(event, event.dates, empty_dates, empty_condition, empty_duration).first.dtstart
        end
      end

      def dtend(event)
        if event.recurring?
          Mhc::OccurrenceEnumerator.new(event, empty_dates, empty_dates, event.recurrence_condition, event.duration).first.dtend
        else
          Mhc::OccurrenceEnumerator.new(event, event.dates, empty_dates, empty_condition, empty_duration).first.dtend
        end
      end

      def rdates(event)
        return nil if event.dates.empty?
        ocs = Mhc::OccurrenceEnumerator.new(event, event.dates, empty_dates, empty_condition, empty_duration).map {|oc| oc.dtstart}
        if event.recurring?
          ocs
        else
          ocs = ocs[1..-1]
          return nil if ocs.empty?
          return ocs
        end
      end

      def exdates(event)
        return nil if event.exceptions.empty?
        ocs = Mhc::OccurrenceEnumerator.new(event, event.exceptions, empty_dates, empty_condition, empty_duration).map {|oc| oc.dtstart }
        return ocs
      end

      def empty_duration
        Mhc::PropertyValue::Range.new(Mhc::PropertyValue::Date)
      end

      def empty_dates
        Mhc::PropertyValue::List.new(Mhc::PropertyValue::Range.new(Mhc::PropertyValue::Date.new))
      end

      def empty_condition
        Mhc::PropertyValue::RecurrenceCondition.new
      end

      def created(event)
        if event.path
          File.ctime(event.path)
        else
          ::Time.utc(2014, 1, 1)
        end
      end

      def last_modified(event)
        if event.path
          File.mtime(event.path)
        else
          ::Time.utc(2014, 1, 1)
        end
      end
    end # class Icalendar

    class IcalendarImporter
      def self.new_from_ics(ics)
        # * 3.8.1.  Descriptive Component Properties
        # ** CATEGORIES  3.8.1.2.  Categories
        # ** DESCRIPTION  3.8.1.5.  Description
        # ** LOCATION  3.8.1.7.  Location
        # ** SUMMARY  3.8.1.12. Summary
        # * 3.8.2.  Date and Time Component Properties
        # ** DTEND  3.8.2.2.  Date-Time End
        # ** DTSTART  3.8.2.4.  Date-Time Start
        # ** DURATION  3.8.2.5.  Duration
        # * 3.8.4.  Relationship Component Properties
        # ** RECURRENCE-ID  3.8.4.4.  Recurrence ID
        # * 3.8.5.  Recurrence Component Properties
        # ** EXDATE  3.8.5.1.  Exception Date-Times
        # ** RDATE  3.8.5.2.  Recurrence Date-Times
        # ** RRULE  3.8.5.3.  Recurrence Rule
        # * 3.8.7.  Change Management Component Properties
        # ** SEQUENCE  3.8.7.4.  Sequence Number
        # * 3.8.8.  Miscellaneous Component Properties
        # ** X-FIELD  3.8.8.2.  Non-Standard Properties

        # DTSTART:
        #   Date part => X-SC-Duration: .first
        #   Time part => X-SC-Time: .first
        # DTEND
        #   Date part =>
        #     DTEND - DTSTART = 1day
        #     DTEND - DTSTART > 1days
        #       Day:
        # RRULE:
        #  X-SC-Cond:
        # UNTIL:
        #  X-SC-Duration: .last
        # RDATES:
        #  X-SC-Day:
        # EXDATES:
        #   X-SC-Day: !YYYYMMDD
        #
        ical = RiCal.parse_string(ics).first
        return nil unless ical

        iev = ical.events.first
        ev = self.new

        allday = !iev.dtstart.respond_to?(:hour)

        ev.uid         = iev.uid
        ev.sequence    = iev.sequence.to_i
        ev.categories  = iev.categories.to_a.join(" ")
        ev.subject     = iev.summary
        ev.location    = iev.location
        ev.description = iev.description

        #ev.recurrence_condition = iev.rrule_property

        if iev.rrule.first.to_s != "" # X-SC-Duration is only for recurring articles
          duration_string = iev.dtstart.to_time.strftime("%Y%m%d") + "-"
          if iev.rrule.first.to_s.match(/until=(\d+)(T\d{6}Z)?/i)
            duration_string += $1
          end
          ev.duration = duration_string
        end

        # ev.created       = iev.created
        # ev.last_modified = iev.last_modified

        ev.dates       = iev.dtstart.to_time.strftime("%Y%m%d") + "-" + (iev.dtend - 1).to_time.strftime("%Y%m%d")
        unless allday
          ev.time_range = iev.dtend.strftime("%H:%m") + '-' + iev.dtend.strftime("%H:%m")
        end
        return ev
      end

    end # IcalendarImporter
  end # Converter
end # Mhc
