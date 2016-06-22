module Mhc
  module PropertyValue
    class ParseError < StandardError; end
    class Base
      def self.parse(string)
        return self.new.parse(string)
      end

      def parse(string)
        @value = string
        return self
      end

      def to_mhc_string
        return @value.to_s
      end

      def empty?
        return true if @value.nil? || (@value.respond_to?(:empty?) && @value.empty?)
        return false
      end

      alias_method :to_s, :to_mhc_string
    end

    dir = File.dirname(__FILE__) + "/property_value"

    autoload :Date,                "#{dir}/date.rb"
    autoload :DateTime,            "#{dir}/date_time.rb"
    autoload :Integer,             "#{dir}/integer.rb"
    autoload :List,                "#{dir}/list.rb"
    autoload :Period,              "#{dir}/period.rb"
    autoload :Range,               "#{dir}/range.rb"
    autoload :RecurrenceCondition, "#{dir}/recurrence_condition.rb"
    autoload :Text,                "#{dir}/text.rb"
    autoload :Time,                "#{dir}/time.rb"

  end # modlue PropertyValue
end # module Mhc
