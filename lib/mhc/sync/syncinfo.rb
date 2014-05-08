################################################################
# Log maintenance functions.
#
# M 2000-04-25 00:06:08 <20.nom@.nomcom> ~nom/Mail/schedule/2000/04/1 Luncheon
# D 2000-04-25 00:06:08 <20.nom@.nomcom> ~nom/Mail/schedule/2000/04/1 Luncheon
# S 2000-04-25 00:06:08 user_id
#
module Mhc
  class Log

    def initialize(filename)
      @filename = filename
    end

    def add_entry(entry)
      file = File.open(@filename, "a+")
      file.print "#{entry}\n"
      file.fsync if file.respond_to?("fsync")
      file.close
    end

    def each_entry
      begin
        file = File.open(@filename)
        while line = file.gets
          yield(MhcLogEntry.new(line.chomp))
        end
        file.close
      rescue
      end
    end

    def entries()
      arry = []
      each_entry{|e|
        arry << e
      }
      return arry
    end

    def shrink_entries(user_id)
      hash = {}
      each_entry{|e|
        if e.status == 'S' and e.rec_id == user_id
          hash.clear
        else
          hash[e.rec_id] = e
        end
      }
      return hash.values
    end
  end # class Log
end # module Mhc

################
module Mhc
    class LogEntry
    attr :status
    attr :mtime
    attr :rec_id
    attr :path
    attr :subject

    def initialize(status, mtime = nil, rec_id = nil, path = nil, subject = nil)
      if mtime.nil?
        init_from_string(status)
      else
        @status, @mtime, @rec_id, @path, @subject =
          status, mtime, rec_id, path, subject
      end
    end

    def to_s
      return [
        @status,
        @mtime.strftime("%Y-%m-%d %H:%M:%S"),
        @rec_id,
        @path,
        @subject
      ].join(' ')
    end

    ################
    private
    ################
    def init_from_string(line)
      str = line.chomp
      status, yymmdd, hhmmss, rec_id, path, subject = str.split
      yy, mm, dd = yymmdd.split('-')
      h,  m,  s  = hhmmss.split(':')

      mtime = ::Time.local(yy.to_i, mm.to_i, dd.to_i,
                          h .to_i, m .to_i, s .to_i)
      @status, @mtime, @rec_id, @path, @subject =
        status, mtime, rec_id, path, subject
    end
  end # class LogEntry
end # module Mhc
