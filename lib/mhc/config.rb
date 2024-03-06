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
      # attr_accessor :name

      def self.create_from_yaml_file(yaml_file)
        yaml_string = File.open(File.expand_path(yaml_file)).read
        return create_from_yaml_string(yaml_string, yaml_file)
      end

      def self.create_from_yaml_string(yaml_string, filename = nil)
        hash = if YAML.respond_to?(:unsafe_load)
                 YAML.unsafe_load(yaml_string, filename: filename)
               else
                 YAML.load(yaml_string, filename)
               end || {}
        return new(hash)
      end

      def self.define_syntax(config)
        @syntax = Syntax.new(config)
        @syntax.keyword_symbols.each do |sym|
          attr_accessor sym # XXX: attr_reader is enough?
        end
      end

      def self.syntax
        return @syntax
      end

      def initialize(hash = {})
        @original_hash = hash
        (hash || {}).each do |key, val|
          raise Mhc::ConfigurationError, "config syntax error (#{key})" unless syntax.keyword?(key)
          var = syntax.instance_variable_name(key)
          obj = create_subnode(key, val)
          instance_variable_set(var, obj)
        end
      end

      attr_reader :original_hash

      def get_value(dot_separated_string = nil)
        if dot_separated_string.to_s == ""
          return original_hash
        end

        key, subkey = dot_separated_string.to_s.upcase.split(".", 2)
        subnode = get_subnode(key)

        if subnode.respond_to?(:get_value)
          return subnode.get_value(subkey)
        else
          return subnode.to_s
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
          obj = obj.respond_to?(:to_hash) ? obj.to_hash : obj.to_s
          hash[key] = obj
        end
        return hash
      end

      private
      def syntax
        self.class.syntax
      end

      def get_subnode(key)
        raise Mhc::ConfigurationError, "Invalid key: #{key}" unless syntax.keyword?(key)
        return instance_variable_get(syntax.instance_variable_name(key))
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
        @original_hash = array
        @configs = []
        (array || []).each do |value|
          item = item_class.new(value)
          @configs << item
        end
      end

      def [](key)
        @configs.find {|c| c.name == key}
      end

      alias_method :get_subnode, :[]

      def <<(conf)
        @configs << conf
      end

      def to_hash # XXX: actually, it returns a Array
        return @configs.map {|c| c.respond_to?(:to_hash) ? c.to_hash : c.to_s}
      end

      def each
        @configs.each do |conf|
          yield conf
        end
      end
    end # List

    ## concrete config classes

    class General < Base
      define_syntax :tzid => String,
                    :repository => String
    end # class General

    class SyncChannel < Base
      define_syntax :name => String,
                    :calendar1 => String,
                    :calendar2 => String,
                    :strategy => String
    end # class SyncChannel

    class Calendar < Base
      define_syntax :name => String,
                    :type => String,
                    :user => String,
                    :password => String,
                    :url => String,
                    :filter => Mhc::Query,
                    :modifiers => [Mhc::Modifier]
    end # class Calendar

    # Top-Level Config
    class Top < Base
      define_syntax :general => General,
                    :sync_channels => [SyncChannel],
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
      unless File.exist?(File.expand_path(file_name))
        raise Mhc::ConfigurationError, "config file '#{file_name}' not found"
      end
      begin
        return Top.create_from_yaml_file(file_name)
      rescue Psych::SyntaxError, Mhc::Query::ParseError, Mhc::Modifier::ParseError => e
        raise Mhc::ConfigurationError, e.message
      end
    end

    def self.create_from_string(string)
      begin
        return Top.create_from_yaml_string(string)
      rescue Psych::SyntaxError, Mhc::Query::ParseError, Mhc::Modifier::ParseError => e
        raise Mhc::ConfigurationError, e.message
      end
    end

  end # class Config
end # module Mhc
