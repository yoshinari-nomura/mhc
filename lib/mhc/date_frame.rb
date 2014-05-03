module Mhc
  module DateFrame
    class Dummy
      def initialize(start_date, interval = 1, wkst = 1)
      end
    end

    class Base
      include DateHelper

      def initialize(start_date, interval = 1, wkst = 1)
        @start_date, @interval, @wkst = start_date, interval, wkst
        rewind
      end

      def each
        loop do
          date = self.next
          yield date
        end
      end

      def next(cycles = 1)
        frame = @frame_start
        @frame_start = next_frame_start(cycles)
        return frame
      end

      def peek
        @frame_start
      end

      def rewind
        @frame_start = beginning_of_frame(@start_date)
        return self
      end

      # go forward to the frame in which DATE is involved
      def forward_to(date)
        rewind
        frames = frames_between(@frame_start, date)
        cycles = (frames + (@interval - 1)) / @interval
        self.next(cycles) if cycles > 0
        return self
      end

      private
      def next_frame_start(cycles = 1)
        raise "should be defined in subclasses"
      end

      def beginning_of_frame(date)
        raise "should be defined in subclasses"
      end

      def frames_between(date1, date2)
        raise "should be defined in subclasses"
      end
    end

    class Yearly < Base
      private

      def next_frame_start(cycles = 1)
        @frame_start >> (@interval * 12 * cycles)
      end

      def beginning_of_frame(date)
        beginning_of_year(date)
      end

      def frames_between(date1, date2)
        years_between(date1, date2)
      end
    end

    class Monthly < Base
      private

      def next_frame_start(cycles = 1)
        @frame_start >> (@interval * cycles)
      end

      def beginning_of_frame(date)
        beginning_of_month(date)
      end

      def frames_between(date1, date2)
        months_between(date1, date2)
      end
    end

    class Weekly < Base
      private
      def next_frame_start(cycles = 1)
        @frame_start + (@interval * 7 * cycles)
      end

      def beginning_of_frame(date)
        beginning_of_week(date, @wkst)
      end

      def frames_between(date1, date2)
        (beginning_of_frame(date2) - beginning_of_frame(date1)) / 7
      end
    end

    class Daily < Base
      private

      def next_frame_start(cycles = 1)
        @frame_start + (@interval * cycles)
      end

      def beginning_of_frame(date)
        date
      end

      def frames_between(date1, date2)
        date2 - date1
      end
    end
  end # module DateFrame
end # module Mhc
