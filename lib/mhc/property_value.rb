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

      alias_method :to_s, :to_mhc_string
    end
  end

  subdir = File.dirname(__FILE__) + "/property_value/"
  require subdir + "date.rb"
  require subdir + "integer.rb"
  require subdir + "list.rb"
  require subdir + "period.rb"
  require subdir + "range.rb"
  require subdir + "recurrence_condition.rb"
  require subdir + "text.rb"
  require subdir + "time.rb"
end
