require 'tzinfo'
require 'ri_cal'
require "kconv"
require "mhc/version"
require "mhc/calendar"
require "mhc/converter"
require "mhc/datastore"
require "mhc/event"
require "mhc/formatter"
require "mhc/logger"
require "mhc/occurrence"
require "mhc/occurrence_enumerator"
require "mhc/date_helper"
require "mhc/date_frame"
require "mhc/date_enumerator"
require "mhc/property_value"
require "mhc/query"
require "mhc/sync"
require "mhc/modifier"
require "mhc/config"
require "mhc/builder"
require "mhc/etag"
require "mhc/caldav"

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
end
