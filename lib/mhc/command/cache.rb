module Mhc
  module Command
    class Cache

      def initialize(calendar)
        calendar.events.each do |event|
          range = event.range
          puts "#{range.min.to_mhc_string},#{range.max.to_mhc_string},#{event.uid},#{event.subject}"
        end
      end

    end # class Cache
  end # module Command
end # module Mhc
