# -*- coding: utf-8 -*-

require "date"

module Mhc
  module PropertyValue
    class Date < ::Date

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

      def parse(string)
        if /^\d{8}$/ =~ string
          self.class.parse(string)
        else
          return nil # raise ParseError
        end
      end

      def to_mhc_string
        return strftime("%Y%m%d")
      end

      def to_ics
        return strftime("%Y%m%d")
      end

    end # class Date
  end # module PropertyValue
end # module Mhc
