require "fileutils"
require "pathname"

module Mhc
  class DataStore
    def initialize(basedir)
      unless basedir and File.directory?(File.expand_path(basedir.to_s))
        raise Mhc::ConfigurationError, "datastore directory '#{basedir}' not found"
      end
      @basedir = Pathname.new(File.expand_path(basedir))
      @cache   = Cache.new(File.expand_path("status/cache/events.pstore", @basedir))
    end

    def entries(range: nil, category: nil, recurrence: nil)
      if range
        int_range = range.min.absolute_from_epoch .. range.max.absolute_from_epoch
      end

      Enumerator.new do |yielder|
        ["inbox", "spool", "presets"].each do |slot|
          dir = File.expand_path(slot, @basedir)
          next unless File.directory?(dir)

          Dir.chdir(dir) do
            Dir.foreach(".") do |ent|
              parse_mhcc(ent).each {|ev|
                next if category   && !ev.in_category?(category)
                next if recurrence && !ev.in_recurrence?(recurrence)
                yielder << ev
              } if /\.mhcc$/ =~ ent
              next unless /\.mhc$/ =~ ent
              uid = $`
              cache_entry = @cache.lookup(uid, ent)
              next if range      && !cache_entry.in_range?(int_range)
              next if category   && !cache_entry.in_category?(category)
              next if recurrence && !cache_entry.in_recurrence?(recurrence)
              yielder << Event.parse_file(File.expand_path(ent))
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

    def create(event, draft = false)
      if find_by_uid(event.uid)
        raise "Already exist uid:#{uid} in #{@basedir}"
      end
      path = uid_to_path(event.uid, draft)
      File.open(path, "w") do |f|
        f.write(event.dump)
      end
      return path.to_s
    end

    def update(event, draft = false)
      unless path = uid_to_path(event.uid, draft)
        raise "Not found uid:#{uid} in #{@basedir}"
      end
      File.open(path, "w") do |f|
        f.write(event.dump)
      end
      return path.to_s
    end

    def delete(uid_or_event)
      uid = if uid_or_event.respond_to?(:uid)
              uid_or_event.uid
            else
              uid_or_event
            end
      if path = find_path(uid)
        File.delete(path)
      else
        raise "Not found uid:#{uid} in #{@basedir}"
      end
    end

    # dump cache entry for debug usage
    def each_cache_entry
      @cache.each do |uid, ent|
        yield uid, ent
      end
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

    def uid_to_path(uid, draft = false)
      return @basedir + ('draft/' + uid + '.mhc') if draft
      return @basedir + ('spool/' + uid + '.mhc')
    end

  end # class DataStore
end # module Mhc

module Mhc
  class DataStore
    class Cache
      require 'pstore'

      VERSION = "1"

      def initialize(cache_filename)
        @pstore = PStore.new(cache_filename)
        load
      end

      # dump cache entry for debug usage
      def each
        load unless @db
        @db.each do |uid, ent|
          yield uid, ent
        end
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
          @pstore["version"] = VERSION
        end
        @dirty = false
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
          @db = (@pstore["version"] == VERSION) && @pstore["root"] || {}
        end
        @dirty = false
      end

    end # class Cache

    class CacheEntry
      attr_reader :mtime, :uid, :subject, :location, :categories, :recurrence, :mission, :range

      def initialize(filename)
        @mtime = File.mtime(filename).to_i

        event = Event.parse_file(filename)
        @uid            = event.uid.to_s
        @subject        = event.subject.to_s
        @location       = event.location.to_s
        @categories     = event.categories.map {|c| c.to_s.downcase}
        @recurrence     = event.recurrence_tag.to_s
        @mission        = event.mission_tag.to_s
        @range          = event.range.min.absolute_from_epoch ..
                          event.range.max.absolute_from_epoch
      end

      def in_category?(category)
        @categories.member?(category.downcase)
      end

      def in_range?(range)
        range.min <= @range.max && @range.min <= range.max
      end

      def in_recurrence?(recurrence)
        @recurrence && @recurrence.downcase == recurrence.downcase
      end

    end # class CacheEntry

  end # class DataStore
end # module Mhc
