module Mhc
  class EnumMerger
    include Enumerable

    alias_method :with_index,  :each_with_index
    alias_method :with_object, :each_with_object

    def initialize(&block)
      @enumerators = []
      @enumerators << Enumerator.new(&block) if block
    end

    def <<(o)
      @enumerators << o
      return self
    end

    def each
      rewind
      loop do
        yield self.next
      end
    end

    # def feed ; end

    def next
      raise StopIteration if @enumerators.empty?
      minimum_enumrator.next
    end

    # def next_values ; end

    def peek
      raise StopIteration if @enumerators.empty?
      minimum_enumrator.peek
    end

    # def peek_values ; end

    def rewind
      send_all(:rewind)
    end

    def send_all(method, *args)
      @enumerators.map{|e| e.send(method, *args)}
    end

    private

    def minimum_enumrator
      min_e, min_v = @enumerators.first, nil
      @enumerators.each do |e|
        v = e.peek rescue nil
        if v and (min_v.nil? or v < min_v)
          min_e, min_v = e, v
        end
      end
      return min_e
    end
  end # class EnumMerger

  class DateEnumerator < EnumMerger
    def initialize(start_date:, end_date:, interval: 1, &block)
      @start_date, @end_date, @interval = start_date, end_date, interval
      super(&block)
    end

    def add_yearly_by_day(start_date: @start_date, end_date: @end_date, interval: @interval, month:, nth:, wday:)
      self << YearlyByDay.new(start_date: start_date, end_date: end_date, interval: interval, month: month, nth: nth, wday: wday).to_enum
    end

    def add_yearly_by_monthday(start_date: @start_date, end_date: @end_date, interval: @interval, month:, mday:)
      self << YearlyByMonthday.new(start_date: start_date, end_date: end_date, interval: interval, month: month, mday: mday).to_enum
    end

    def add_monthly_by_day(start_date: @start_date, end_date: @end_date, interval: @interval, nth:, wday:)
      self << MonthlyByDay.new(start_date: start_date, end_date: end_date, interval: interval, nth: nth, wday: wday).to_enum
    end

    def add_monthly_by_monthday(start_date: @start_date, end_date: @end_date, interval: @interval, mday:)
      self << MonthlyByMonthday.new(start_date: start_date, end_date: end_date, interval: interval, mday: mday).to_enum
    end

    def add_weekly(start_date: @start_date, end_date: @end_date, interval: @interval, wday:)
      self << Weekly.new(start_date: start_date, end_date: end_date, interval: interval, wday: wday).to_enum
    end

    def add_by_range_list(start_date: @start_date, end_date: @end_date, range_list:)
      self << ByRangeList.new(start_date: start_date, end_date: end_date, range_list: range_list).to_enum
    end

    ################################################################
    class Base
      include DateHelper

      def initialize(start_date:, end_date:, interval: 1, wkst: 1)
        @start_date, @end_date, @interval, @wkst = start_date, end_date, interval, wkst
        @frame_manager = frame_manager.new(start_date, interval, wkst)
      end

      def each
        head, tail = range
        @frame_manager.forward_to(head).each do |frame|
          break if frame > tail
          date = occurrence_in_frame(frame)
          next unless date
          break if date > tail
          next  if date < head
          yield date
        end
      end

      private

      def range
        s = (@range_from and @start_date < @range_from) ? @range_from : @start_date
        e = (@range_to   and @end_date   > @range_to)   ? @range_to   : @end_date
        return [s, e]
      end

      def frame_manager
        raise "should be defined in subclasses"
      end

      def occurrence_in_frame(date)
        raise "should be defined in subclasses"
      end
    end # class Base

    ################################################################
    # Enumerate yealy dates by day like: Apr 4th Tue
    class YearlyByDay < Base
      def initialize(start_date:, end_date:, interval: 1, month:, nth:, wday:)
        super(start_date: start_date, end_date: end_date, interval: interval)
        @month, @nth, @wday = month, nth, wday
      end

      private

      def frame_manager
        DateFrame::Yearly
      end

      def occurrence_in_frame(date)
        make_date_by_day(year: date.year, month: @month, nth: @nth, wday: @wday) rescue nil
      end
    end # class YearlyByDay

    ################################################################
    # Enumerate yealy dates by month-day like: Apr 22
    class YearlyByMonthday < Base
      def initialize(start_date:, end_date:, interval: 1, month:, mday:)
        super(start_date: start_date, end_date: end_date, interval: interval)
        @month, @mday = month, mday
      end

      private

      def frame_manager
        DateFrame::Yearly
      end

      def occurrence_in_frame(date)
        Mhc::PropertyValue::Date.new(date.year, @month, @mday) rescue nil
      end
    end # class YearlyByMonthday

    ################################################################
    # Enumerate monthly dates by day like: 4th Tue
    class MonthlyByDay < Base
      def initialize(start_date:, end_date:, interval: 1, nth:, wday:)
        super(start_date: start_date, end_date: end_date, interval: interval)
        @nth, @wday = nth, wday
      end

      private

      def frame_manager
        DateFrame::Monthly
      end

      def occurrence_in_frame(date)
        make_date_by_day(year: date.year, month: date.month, nth: @nth, wday: @wday) rescue nil
      end
    end # class MonthlyByDay

    ################################################################
    # Enumerate monthly dates by month-day like: 22
    class MonthlyByMonthday < Base
      def initialize(start_date:, end_date:, interval: 1, mday:)
        super(start_date: start_date, end_date: end_date, interval: interval)
        @mday = mday
      end

      private

      def frame_manager
        DateFrame::Monthly
      end

      def occurrence_in_frame(date)
        Mhc::PropertyValue::Date.new(date.year, date.month, @mday) rescue nil
      end
    end # class MonthlyMonthday

    ################################################################
    # Enumerate weekly dates like: Tue
    class Weekly < Base
      def initialize(start_date:, end_date:, interval: 1, wkst: 1, wday:)
        super(start_date: start_date, end_date: end_date, interval: interval)
        @wday = wday
      end

      private

      def frame_manager
        DateFrame::Weekly
      end

      # Sun Mon Tue Wed Thu Fri Sat Sun Mon Tue ...
      #  0   1   2   3   4   5   6   0   1   2  ...
      def occurrence_in_frame(date)
        bof = date - ((date.wday - @wkst) % 7)
        candidate = bof + (@wday - bof.wday) % 7
        return candidate if date <= candidate
        return nil
      end
    end # class Weekly

    ################################################################
    # Enumerate every n days
    class Daily < Base
      def initialize(start_date:, end_date:, interval:1)
        super(start_date: start_date, end_date: end_date, interval: interval)
      end

      private

      def frame_manager
        DateFrame::Daily
      end

      def occurrence_in_frame(date)
        return date
      end
    end # class Daily

    ################################################################
    # Enumerate dates from list.
    class ByRangeList < Base
      def initialize(start_date:, end_date:, range_list:)
        super(start_date: start_date, end_date: end_date)
        @range_list = range_list
      end

      def each
        head, tail = range
        date_list =  @range_list.map{|range| range.to_a }.flatten
        date_list.each do |date|
          break if date > tail
          next  if date < head
          yield date
        end
      end

      private

      def frame_manager
        DateFrame::Dummy
      end
    end # class ByRangeList

  end # class DateEnumerator
end # module Mhc
