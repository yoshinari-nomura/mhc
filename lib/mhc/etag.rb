module Mhc
  class EtagStore
    def initialize(top_directory)
      @top_directory = top_directory
    end

    def put(key, value)
      print "ETAG put #{key} => #{value} in #{@top_directory}\n"
      if value.nil?
        unlink(key)
      else
        store(key, value)
      end
    end

    def get(uid)
      if value = load(uid)
        return value
      end
    end

    def uid_list
      keys
    end

    def report_etags(uids = nil)
      hash = {}
      uid_list.each do |uid|
        hash[uid] = get(uid)
      end
      return hash
    end

    private
    def keys
      Dir.glob(File.join(@top_directory, '*.etag')).map {|p| make_key(p)}
    end

    def store(key, value)
      File.open(make_path(key), "w") do |f|
        f.write(value)
      end
    end

    def load(key)
      begin
        File.open(make_path(key), "r") do |f|
          return f.read
        end
      rescue
        return nil
      end
    end

    def unlink(key)
      if File.exists?(path = make_path(key))
        File.unlink(path)
      end
    end

    def make_path(key)
      File.join(@top_directory, key.to_s + '.etag')
    end

    def make_key(path)
      File.basename(path, '.etag')
    end
  end # class EtagStore
end # module Mhc
