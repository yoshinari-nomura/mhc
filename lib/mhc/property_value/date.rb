# -*- coding: utf-8 -*-

require "date"

module Mhc
  module PropertyValue
    class Date < ::Date

      DAYS_OF_MONTH = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      def self.parse(string)
        if /^\d{8}$/ =~ string
          super(string)
        else
          return nil # raise ParseError
        end
      end

      def self.parse_relative(date_string)
        case (date_string.downcase)
        when 'today'
          return self.today

        when 'tomorrow'
          return self.today.succ

        when /^\d{8}$/
          return self.parse(date_string)

        when /^\d{6}$/
          return self.parse(date_string + '01')

        when /^thismonth$/
          return self.today.first_day_of_month

        when /^nextmonth$/
          return self.today.first_day_of_month.next_month

        else
          raise ParseError
        end
      end

      def self.parse_range(range_string)
        case range_string
        # yyyymmdd-yyyymmdd
        when /^([^+-]+)-([^+-]+)$/
          return parse_relative($1)..parse_relative($2)

        # yyyymmdd+2w
        when /^([^+-]+)\+(\d+)([dwm])$/
          date = parse_relative($1)
          return date..date.succ_by($3, $2.to_i).prev_day

        when /^(thismonth|nextmonth|\d{6})$/
          date = parse_relative($1)
          return date..date.last_day_of_month

        when /^([^+-]+)$/
          date = parse_relative($1)
          return date..date
        else
          raise ParseError
        end
      end

      def succ_by(unit = :d, number = 1)
        case unit.to_sym
        when :d
          return self + number.to_i
        when :w
          return self + (number.to_i * 7)
        when :m
          return self >> number.to_i
        end
      end

      def parse(string)
        if /^\d{8}$/ =~ string
          self.class.parse(string)
        else
          return nil # raise ParseError
        end
      end

      def add_time(time = nil)
        if time
          return ::Time.local(year, month, mday, time.hour, time.minute)
        else
          return ::Time.local(year, month, mday, 0, 0)
        end
      end

      def to_mhc_string
        return strftime("%Y%m%d")
      end

      def last_week_of_month?
        return mday > days_of_month - 7
      end

      def week_number_of_month
        return (mday - 1) / 7 + 1
      end

      def days_of_month
        return DAYS_OF_MONTH[month] + (month == 2 && leap? ? 1 : 0)
      end

      def first_day_of_month
        return self.class.new(year, month, 1)
      end

      def last_day_of_month
        return self.class.new(year, month, -1)
      end

      def each_day_in_month
        for d in (1 .. days_of_month)
          yield self.class.new(year, month, d)
        end
      end

      def today?
        return self.class.today == self
      end

      def absolute_from_epoch
        return self - Date.new(1970, 1, 1)
      end

      #
      # Make a date by DAY like ``1st Wed of Nov, 1999''.
      # caller must make sure:
      #   YEAR and MONTH must be valid.
      #   NTH must be <0 or >0.
      #   WDAY must be 0..6.
      #
      # returns nil if no date was match (for example,
      # no 5th Saturday exists on April 2010).
      #
      def self.new_by_day(year, month, nth, wday)
        return nil if nth < -5 or nth > 5 or nth == 0
        direction = nth > 0 ? 1 : -1

        edge      = Date.new(year, month, direction)
        y_offset  = nth - direction
        x_offset  = wday_difference(edge.wday, wday, direction)
        mday      = edge.mday + y_offset * 7 + x_offset

        return new(year, month, mday) # May raise ArgumentError
      end

      def next_monthday(month, mday)
        year = self.year + (month < self.month ? 1 : 0)
        return self.class.new(year, month, mday)
      end

      def next_day(month, nth, wday)
        year = self.year + (month < self.month ? 1 : 0)
        year += 1 while !(date = self.class.new_by_day(year, month, nth, wday))
        return date
      end

      def to_ics
        return strftime("%Y%m%d")
      end

      private
      #
      # Returns diff of days between 2 wdays: FROM and TO.
      # Each FROM and TO is one of 0(=Sun) ... 6(Sat).
      #
      # DIRECTION must be -1 or 1, which represents search direction.
      #
      #     Sun Mon Tue Wed Thu Fri Sat Sun Mon Tue ...
      #      0   1   2   3   4   5   6   0   1   2  ...
      #
      # returns  3 if FROM, TO, DIRECTION = 4, 0,  1
      # returns -4 if FROM, TO, DIRECTION = 4, 0, -1
      #
      def wday_difference(from, to, direction)
        return direction * ((direction * (to - from)) % 7)
      end

    end # class Date
  end # module PropertyValue
end # module Mhc
