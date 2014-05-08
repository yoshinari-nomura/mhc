module Mhc
  module Sync
    ##
    # It wraps existing database to adds ability to manage etag cache for sync status tracking.
    # The downstream database is supposed to respond to:
    #
    # 1. report_etags
    #      report_etags(uids = nil) # returns one of:
    #       => {uid_string => etag_object } # Hash
    #       => {uid_string => etag_string } # Hash
    #       => [uid_etag_object] # Array
    #
    #    uid_etag_object is an object which respond to #etag and #uid method.
    #    etag_object is an object which respond to #etag method.
    #
    # 2. get_with_etag
    #      get_with_etag(uid)
    #      # => [RECORD, etag]
    #
    #    Each RECORD has to respond to #to_ics_string or #body that
    #    returns iCalendar-conformed string.
    #
    # 3. put_if_match
    #      put_if_match(uid, ics_string, expected_etag)
    #      # => put ics_string if "ETAG" equals to expected_etag
    #
    # 4. delete_if_match
    #      delete_if_match(uid, expected_etag)
    #      # => delete uid if "ETAG" equals to expected_etag
    #
    # expected_etag is a string for sync.  if expected_etag is omitted
    # (or nil), put_if_match and delete_if_match will ignore
    # conflictions.
    #
    class StatusManager
      def initialize(real_db, etag_db)
        @db, @etag_db = real_db, etag_db
        refresh_status
      end

      def syncinfo(uid)
        Status.new(uid, self)
      end

      def uid_list
        (@db_status.keys + @etag_status.keys).sort.uniq
      end

      ##
      # delegation to original DB with sync-status check.
      #
      # To make sure any kind of update is safe,
      # you may want to check current_record.ex_etag == current_record.etag
      # (means current_record.unmodified? is true) like:
      #
      #    if current_record.unmodified?
      #      @db.put(new_record)
      #    end
      #
      # However, this will not work. Because this check-and-update
      # must be an atomic operation.
      # So, instead, we have to do:
      #
      #   @db.put_if_match(new_record, current_record.ex_etag)
      #
      def get(uid)
        res, etag = @db.get_with_etag(uid)
        @db_status[uid] = etag if etag
        return Status.new(uid, self, res)
      end

      def put(modified_record)
        current_record = syncinfo(modified_record.uid)
        if @db.put_if_match(current_record.uid, modified_record.to_ics_string, current_record.ex_etag)
          ## XXX: put_if_match should return the new etag value, and we
          ## have to use it as a new etag for the current record.
          ## However, some CalDAV servers (Google Calendar) do not
          ## return any etag on PUT.
          ## So, we have to PROPFIND immediately after the
          ## PUT. This is a small crack on atomicity
          refresh_status(current_record.uid)  # refresh propfind cache.
          current_record.mark_synced
          return Status.new(current_record.uid, self)
        else
          return nil # put failed.
        end
      end

      def delete(uid)
        current_record = syncinfo(uid)
        if @db.delete_if_match(current_record.uid, current_record.ex_etag)
          refresh_status(current_record.uid)
          current_record.mark_synced
          return Status.new(current_record.uid, self)
        else
          return nil
        end
      end

      # propfind with cache
      def refresh_status(uids = :all)
        ### XXX: care fore UIDs for partial update.
        @db_status = make_hash(@db.report_etags)
        @etag_status = make_hash(@etag_db.report_etags)
      end

      def etag(uid)
        if @db_status[uid].respond_to?(:etag)
          return @db_status[uid].etag
        else
          return @db_status[uid]
        end
      end

      def ex_etag(uid)
        @etag_status[uid]
      end

      def mark_synced(uid, etag)
        @etag_db.put(uid, etag)
        return self
      end

      private
      ## etag_report is one of:
      ##   + {uid_string => etag_object } style hash,
      ##   + {uid_string => etag_string } style hash,
      ##   + [uid_etag_object] style array,
      ## uid_etag_object is an object which respond to #etag and #uid method.
      ## etag_object is an object which respond to #etag method.
      def make_hash(etag_report)
        return etag_report if etag_report.respond_to?(:keys)
        hash = {}
        etag_report.map {|o| hash[o.uid] = o.etag} if etag_report
        return hash
      end

    end # class StatusManager
  end # module Sync
end # module Mhc
