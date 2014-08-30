require 'tzinfo'
require 'ri_cal'
require "kconv"

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
  autoload :Version,              "#{dir}/version.rb"
end
