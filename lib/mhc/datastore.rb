require "fileutils"

module Mhc
  class DataStore
    def initialize(basedir)
      unless basedir and File.directory?(File.expand_path(basedir.to_s))
        raise Mhc::ConfigurationError, "datastore directory '#{basedir}' not found"
      end
      @basedir = Pathname.new(File.expand_path(basedir))
      @cache   = Cache.new(File.expand_path("status/cache/events.pstore", @basedir))
    end

    def entries(date_range = nil)
      if date_range
        int_range = date_range.min.absolute_from_epoch .. date_range.max.absolute_from_epoch
      end

      Enumerator.new do |yielder|
        ["inbox", "spool", "presets"].each do |slot|
          dir = File.expand_path(slot, @basedir)
          next unless File.directory?(dir)

          Dir.chdir(dir) do
            Dir.foreach(".") do |ent|
              parse_mhcc(ent).each {|ev| yielder << ev} if /\.mhcc$/ =~ ent
              next unless /\.mhc$/ =~ ent
              uid = $`
              cache_entry = @cache.lookup(uid, ent)
              if !date_range || cache_entry.involved?(int_range)
                yielder << Event.parse_file(File.expand_path(ent))
              end
            end
          end
        end
        @cache.save
      end
    end

    def logger
      @logger ||= Mhc::Logger.new(@logfile)
    end

    def find_by_uid(uid)
      path = find_path(uid)
      return nil unless path
      return Event.parse_file(path)
    end

    ################################################################
    private

    def parse_mhcc(filename)
      string = File.open(filename).read.scrub.gsub(/^\s*#.*$/, "").strip
      string.split(/\n\n\n*/).map do |header|
        Event.parse(header)
      end
    end

    def find_path(uid)
      glob = @basedir + ('**/' + uid + '.mhc')
      return Dir.glob(glob).first
    end

    def uid_to_path(uid)
      return @uid_pool + uid
    end

  end # class DataStore
end # module Mhc

module Mhc
  class DataStore
    class Cache
      require 'pstore'

      def initialize(cache_filename)
        @pstore = PStore.new(cache_filename)
        load
      end

      def lookup(uid, filename)
        unless c = get(uid) and File.mtime(filename).to_i <= c.mtime
          c = CacheEntry.new(filename)
          put(uid, c)
        end
        return c
      end

      def save
        return self unless @dirty
        @pstore.transaction do
          @pstore["root"] = @db
        end
      end

      private

      def get(uid)
        @db[uid]
      end

      def put(uid, value)
        @db[uid] = value
        @dirty = true
      end

      def load
        @pstore.transaction do
          @db = @pstore["root"] || {}
        end
      end

    end # class Cache

    class CacheEntry
      attr_reader :mtime, :range

      def initialize(filename)
        @mtime, event = File.mtime(filename).to_i, Event.parse_file(filename)
        @range = event.range.min.absolute_from_epoch ..
                 event.range.max.absolute_from_epoch
      end

      def involved?(range)
        range.min <= @range.max && @range.min <= range.max
      end

    end # class CacheEntry

  end # class DataStore
end # module Mhc
