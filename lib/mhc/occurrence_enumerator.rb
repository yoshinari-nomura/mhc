module Mhc
  class OccurrenceEnumerator
    include Enumerable
    # ; The FREQ rule part is REQUIRED,
    # ; but MUST NOT occur more than once.
    #
    # FREQ       = (DAILY|WEEKLY|MONTHLY|YEARLY)
    #
    # ; The UNTIL or COUNT rule parts are OPTIONAL,
    # ; but they MUST NOT occur in the same 'recur'.
    #
    # UNTIL      = (date|date-time)
    # COUNT      = \d+
    #
    # ; The other rule parts are OPTIONAL,
    # ; but MUST NOT occur more than once.
    #
    # INTERVAL   = \d+ # positive value default is 1
    # BYDAY      = ([+-]\d{1,2})?(SU|MO|TU|WE|TH|FR|SA),... # 1 to 53
    # BYMONTHDAY = [+-]\d{1,2},...   # 1 to 31
    # BYYEARDAY  = [+-]\d{1,3},...   # 1 to 366
    # BYWEEKNO   = [+-]\d{1,3},...   # 1 to 53
    # BYMONTH    = \d{1,2},...       # 1 to 12
    # BYSETPOS   = [+-]\d+,...       # 1 to 366
    # WKST       = (SU|MO|TU|WE|TH|FR|SA)
    #
    def initialize(event, dates, exceptions, recurrence_condition, duration, range = nil)
      @event = event

      # Since some articles with RECURRENCE_CONDITION and without DURATION
      # makes infinit entries, we have to clip the range by some artificial values
      # It will make 101 enum entries from 1970-1-1 to now+50 years:
      #
      #   X-SC-Subject: New Year's Day
      #   X-SC-Cond: Jan 1
      #
      range = (Date.new(1970, 1, 1) .. Date.new(Date.today.year + 50)) unless range

      # If we have both DURATION and RANGE, we can take narrower term
      # by the combination of the both.
      range = duration.narrow(range.first, range.last)

      # range.last is effective in narrowing the end_date,
      # however, we can't adopt range.first to the start_date.
      # Original start_date derived from DURATION is required for calculating
      # the start point of recurrence loop in case the
      # loop interval is larger than one.
      #
      # At moment, we will have over-scanning entries even if the range.first
      # is set narrower than duration.first
      #
      # we need some good way to pass the both duration.first and range.first
      # to the down-stream enumerators.
      #
      end_date   = range.last
      start_date = duration.first || range.first

      @enumerator = Mhc::DateEnumerator.new(start_date: start_date, end_date: range.last)
      condition_to_enumerator(@enumerator, recurrence_condition, start_date, range.last)
      @enumerator.add_by_range_list(range_list: dates)
      @exceptions = exceptions.map{|range| range.to_a }.flatten
    end

    def each
      @enumerator.each do |date|
        next if @exceptions.include?(date)
        yield Mhc::Occurrence.new(@event, date)
      end
    end

    private

    def condition_to_enumerator(enumerator, cond, start_date, end_date)
      if cond.yearly?
        cond.cond_mon.each do |mon|
          cond.cond_ord.each do |ord|
            cond.cond_wek.each do |wek|
              enumerator.add_yearly_by_day(month: mon, nth: ord, wday: wek)
            end
          end
          cond.cond_num.each do |num|
            enumerator.add_yearly_by_monthday(month: mon, mday: num)
          end
        end
      elsif cond.monthly?
        cond.cond_ord.each do |ord|
          cond.cond_wek.each do |wek|
            enumerator.add_monthly_by_day(nth: ord, wday: wek)
          end
        end
        cond.cond_num.each do |num|
          enumerator.add_monthly_by_monthday(mday: num)
        end
      elsif cond.weekly?
        cond.cond_wek.each do |wek|
          enumerator.add_weekly(wday: wek)
        end
      end
      return enumerator
    end
  end # class OccurrenceEnumerator
end # module Mhc
