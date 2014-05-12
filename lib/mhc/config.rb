require 'yaml'
require 'pp'

module Mhc
  class Config
    # Syntax table manipulation
    class Syntax
      def initialize(syntax_config)
        @syntax_config = syntax_config
      end

      def keyword_symbols
        @syntax_config.keys
      end

      def keywords
        keyword_symbols.map {|sym| sym.to_s.upcase }
      end

      def keyword?(word)
        if word.is_a?(Symbol)
          keyword_symbols.member?(word)
        else
          # String
          keywords.member?(word)
        end
      end

      def instance_variable_name(word)
        return nil unless keyword?(word)
        return '@' + as_symbol(word).to_s
      end

      def item_class(word)
        return nil unless keyword?(word)
        @syntax_config[as_symbol(word)]
      end

      private
      def as_symbol(word)
        word.to_s.downcase.sub(/^@+/, "").to_sym
      end
    end # class Syntax

    # Parse Key-Value object in YAML
    class Base
      attr_accessor :name

      def self.create_from_yaml_file(yaml_file)
        yaml_string = File.open(File.expand_path(yaml_file)).read
        return create_from_yaml_string(yaml_string, yaml_file)
      end

      def self.create_from_yaml_string(yaml_string, filename = nil)
        hash = YAML.load(yaml_string, filename) || {}
        return new(hash)
      end

      def self.define_syntax(config)
        @syntax = Syntax.new(config)
        @syntax.keyword_symbols.each do |sym|
          attr_accessor sym
        end
      end

      def self.syntax
        return @syntax
      end

      def initialize(hash = {})
        hash.each do |key, val|
          raise "config syntax error (#{key})" unless syntax.keyword?(key)
          var = syntax.instance_variable_name(key)
          obj = create_subnode(key, val)
          instance_variable_set(var, obj)
        end
      end

      def to_yaml
        return self.to_hash.to_yaml
      end

      def to_hash
        hash = {}
        syntax.keywords.each do |key|
          var = syntax.instance_variable_name(key)
          obj = instance_variable_get(var)
          obj = obj.to_hash if obj.respond_to?(:to_hash)
          hash[key] = obj
        end
        return hash
      end

      private
      def syntax
        self.class.syntax
      end

      def create_subnode(keyword, value)
        item_class = syntax.item_class(keyword)
        if item_class.is_a?(Array)
          return List.new(item_class.first, value)
        elsif item_class == String
          return value.to_s
        else
          return item_class.new(value)
        end
      end

    end # class Base

    # Parse Array object in YAML
    class List < Base
      include Enumerable

      def initialize(item_class, array = [])
        @configs = []
        array.each do |value|
          item = item_class.new(value)
          @configs << item
        end
      end

      def [](key)
        @configs.find {|c| c.name == key}
      end

      def <<(conf)
        @configs << conf
      end

      def to_hash # XXX: actually, it returns a Array
        return @configs.map {|c| c.to_hash}
      end

      def each
        @configs.each do |conf|
          yield conf
        end
      end
    end # List

    ## concrete config classes

    class SyncChannel < Base
      define_syntax :name => String,
                    :calendar1 => String,
                    :calendar2 => String,
                    :strategy => String
    end # class SyncChannel

    class Calendar < Base
      define_syntax :name => String,
                    :type => String,
                    :repository => String,
                    :user => String,
                    :password => String,
                    :url => String,
                    :filter => Mhc::Query
    end # class Calendar

    # Top-Level Config
    class Top < Base
      define_syntax :sync_channels => [SyncChannel],
                    :calendars => [Calendar]

      def embed_values
        super hash
        self.sync_channels.each do |ch|
          # String -> Calendar
          ch.calendar1 = calendars[ch.calendar1] if calendars[ch.calendar1]
          ch.calendar2 = calendars[ch.calendar2] if calendars[ch.calendar2]
        end
      end
    end # class Top

    def self.create_from_file(file_name)
      unless File.exists?(File.expand_path(file_name))
        raise "config file '#{file_name}' not found"
      end
      return Top.create_from_yaml_file(file_name)
    end

    def self.create_from_string(string)
      return Top.create_from_yaml_string(string)
    end

  end # class Config
end # module Mhc
