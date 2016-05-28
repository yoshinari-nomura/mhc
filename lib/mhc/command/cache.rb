module Mhc
  module Command
    class Cache

      def initialize(datastore)
        epoch = Date.new(1970, 1, 1)
        puts"UID,MTIME,MIN,MAX,CATEGORIES,RECURRENCE,SUBJECT"
        datastore.each_cache_entry do |uid, ent|
          puts"#{ent.uid},#{ent.mtime},#{epoch + ent.range.min},#{epoch + ent.range.max},#{(ent.categories||[]).join(' ')},#{ent.recurrence},#{ent.subject}"
        end
      end

    end # class Cache
  end # module Command
end # module Mhc
