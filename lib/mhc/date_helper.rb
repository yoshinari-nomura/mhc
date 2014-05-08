module Mhc
  module DateHelper
    # Make a date by DAY like ``1st Wed of Nov, 1999''.
    # caller must make sure:
    #   YEAR and MONTH must be valid.
    #   NTH must be <0 or >0.
    #   WDAY must be 0..6.
    #
    # returns nil if no date matches (for example, no 5th Saturday
    # exists on April 2010).
    #
    private
    def make_date_by_day(year:, month:, nth:, wday:)
      direction = nth > 0 ? 1 : -1

      edge  = Mhc::PropertyValue::Date.new(year, month, direction)
      ydiff = nth - direction
      xdiff = direction * ((direction * (wday - edge.wday)) % 7)
      mday  = edge.mday + ydiff * 7 + xdiff

      raise ArgumentError if mday < 1
      return Mhc::PropertyValue::Date.new(year, month, mday)
    end

    def beginning_of_year(date)
      date.class.new(date.year, 1, 1)
    end

    def beginning_of_month(date)
      date.class.new(date.year, date.month, 1)
    end

    def beginning_of_week(date, wkst = 1)
      date - ((date.wday - wkst) % 7)
    end

    def years_between(date1, date2)
      date2.year - date1.year
    end

    def months_between(date1, date2)
      (date2.year * 12 + date2.month) - (date1.year * 12 + date1.month)
    end

    def weeks_between(date1, date2)
      (date2 - date1 + 6) / 7
    end
  end # module DateHelper
end # module Mhc
