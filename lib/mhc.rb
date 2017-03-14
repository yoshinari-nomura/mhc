require 'tzinfo'
require 'ri_cal'
require "kconv"

## Monkey patch to the original RiCal https://github.com/rubyredrick/ri_cal
## delived from:
##   git clone https://github.com/yoshinari-nomura/ri_cal.git
##   git diff 369a4ee..cdb1f75
##
module RiCal
  class Component #:nodoc:

    def method_missing(selector, *args, &b) #:nodoc:
      xprop_candidate = selector.to_s
      if (match = /^(x_[^=]+)(=?)$/.match(xprop_candidate))
        x_property_key = match[1].gsub('_','-').upcase
        if match[2] == "="
          args.each do |val|
            add_x_property(x_property_key, val)
          end
        else
          x_properties[x_property_key].map {|property| property.ruby_value}
        end
      else
        super
      end
    end # def method_missing

    def export_x_properties_to(export_stream) #:nodoc:
      x_properties.each do |name, props|
        props.each do | prop |
          export_stream.puts("#{name}#{prop.to_s}")
        end
      end
    end # def export_x_properties_to

  end # class Component

  class PropertyValue
    class OccurrenceList < Array

      def visible_params # :nodoc:
        result = params.dup

        case @elements.first
        when Date
          result = {"VALUE" => "DATE"}.merge(params)
        when DateTime
          result = {"VALUE" => "DATE-TIME"}.merge(params)
        when Period
          result = {"VALUE" => "PERIOD"}.merge(params)
        end

        if has_local_timezone?
          result['TZID'] = tzid
        else
          result.delete('TZID')
        end
        result
      end
    end
  end # class PropertyValue

end # module RiCal

module Mhc # :nodoc:
  def self.default_tzid=(tzid)
    @tzid = tzid
    ENV["MHC_TZID"] = tzid
    RiCal::PropertyValue::DateTime.default_tzid = tzid
  end

  def self.default_tzid
    @tzid
  end

  if ENV["MHC_TZID"]
    self.default_tzid = ENV["MHC_TZID"]
  end

  class ConfigurationError < StandardError ; end

  dir = File.dirname(__FILE__) + "/mhc"

  autoload :Builder,              "#{dir}/builder.rb"
  autoload :CalDav,               "#{dir}/caldav.rb"
  autoload :Calendar,             "#{dir}/calendar.rb"
  autoload :Command,              "#{dir}/command.rb"
  autoload :Config,               "#{dir}/config.rb"
  autoload :Converter,            "#{dir}/converter.rb"
  autoload :DataStore,            "#{dir}/datastore.rb"
  autoload :DateEnumerator,       "#{dir}/date_enumerator.rb"
  autoload :DateFrame,            "#{dir}/date_frame.rb"
  autoload :DateHelper,           "#{dir}/date_helper.rb"
  autoload :EtagStore,            "#{dir}/etag.rb"
  autoload :Event,                "#{dir}/event.rb"
  autoload :Formatter,            "#{dir}/formatter.rb"
  autoload :Logger,               "#{dir}/logger.rb"
  autoload :Modifier,             "#{dir}/modifier.rb"
  autoload :Occurrence,           "#{dir}/occurrence.rb"
  autoload :OccurrenceEnumerator, "#{dir}/occurrence_enumerator.rb"
  autoload :PropertyValue,        "#{dir}/property_value.rb"
  autoload :Query,                "#{dir}/query.rb"
  autoload :Sync,                 "#{dir}/sync.rb"
  autoload :VERSION,              "#{dir}/version.rb"
  autoload :PRODID,               "#{dir}/version.rb"
end
