require "fileutils"

module Mhc
  # DataStore provides simple key-value store using background file system.
  # keys and values are simply mapped to filename and their contents.
  #
  class DataStore
    def initialize(basedir)
      unless basedir and File.directory?(File.expand_path(basedir.to_s))
        raise Mhc::ConfigurationError, "datastore directory '#{basedir}' not found"
      end
      @basedir   = Pathname.new(File.expand_path(basedir))
      @slot_top  = @basedir
      @uid_top   = @basedir + "db/uid"
      @logfile   = @basedir + 'db/mhc-db-transaction.log'
    end

    def yield_mhcc(yielder, filename)
      string = File.open(filename, "r"){|f| f.read }
      string.scrub!
      string.gsub!(/^\s*#.*$/, "") # strip comments
      string.strip!
      string.split(/\n\n\n*/).each do |header|
        yielder << [nil, header]
      end
    end

    def entries(slot = nil)
      paths = slot_to_path_list(slot)

      Enumerator.new do |yielder|
        paths.each do |path|
          next unless File.directory?(path)
          Dir.glob(path + "/**/*.mhc*").each do |ent|
            if ent =~ /\.mhcc$/
              yield_mhcc(yielder, ent)
            else
              yielder << [ent, File.open(ent, "r"){|f| f.gets("\n\n") }]
            end
          end
        end
      end
    end

    def logger
      @logger ||= Mhc::Logger.new(@logfile)
    end

    def store(uid, slot, data)
      path = new_path_in_slot(slot)
      store_data(path, data)
      store_uid(uid, path)
    end

    def find_by_uid(uid)
      path = find_path(uid)
      return nil unless path

      File.open(path, "r") do |file|
        return file.read
      end
    end

    def delete(uid)
      find_path(uid)
    end

    private

    def store_data(path, data)
      File.open(path, "w") do |file|
        file.write(data)
      end
    end

    def store_uid(uid, path)
      File.open(@uid_top + uid, "w") do |file|
        file.write(path)
      end
    end

    def find_path(uid)
      glob = @slot_top + ('**/' + uid + '.mhc')
      return Dir.glob(glob).first
    end

    def uid_to_path(uid)
      return @uid_pool + uid
    end

    def slot_to_path_list(slot = nil)
      if slot
        [File.expand_path(slot.to_s, @slot_top)]
      else
        Dir.glob(File.expand_path("{[0-9][0-9][0-9][0-9],inbox,intersect,presets}", @slot_top)).to_a
      end
    end

    def new_path_in_slot(slot)
      return nil if !makedir_or_higher(slot)
      new = 1
      Dir.open(slot).each do |file|
        if (file =~ /^\d+$/)
          num = file.to_i
          new = num + 1 if new <= num
        end
      end
      return slot + '/' + new.to_s
    end

    def makedir_or_higher(dir)
      return true if File.directory?(dir)
      parent = File.dirname(dir)
      if makedir_or_higher(parent)
        Dir.mkdir(dir)
        File.open(dir, "r") {|f| f.sync} if File.method_defined?("fsync")
        return true
      end
      return false
    end
  end # class DataStore
end # module Mhc
