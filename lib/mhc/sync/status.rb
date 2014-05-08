module Mhc
  module Sync
    ##
    # status
    #
    class Status
      def initialize(uid, manager, wrapped_record = nil)
        @uid, @manager, @wrapped_record = uid, manager, wrapped_record
      end

      def uid
        return @uid
      end

      def etag
        @manager.etag(@uid)
      end

      def ex_etag
        @manager.ex_etag(@uid)
      end

      def sync_status
        return :norecord   if !etag and !ex_etag
        return :created    if  etag and !ex_etag
        return :deleted    if !etag and  ex_etag
        return :unmodified if  etag ==   ex_etag
        return :modified   if  etag !=   ex_etag
      end

      def deleted?
        sync_status == :deleted
      end

      def unmodified?
        sync_status == :unmodified or sync_status == :norecord
      end

      def modified?
        !unmodified?
      end

      def mark_synced(etag = self.etag)
        @manager.mark_synced(uid, etag)
        return self
      end

      ### as a calendar DB redord
      def to_ics_string
        # LastNote or mhc is assumed.
        if  @wrapped_record.respond_to?(:to_ics_string)
          result = @wrapped_record.to_ics_string
           return result
        end

        # HTTP::Response from caldav server is assumed.
        if @wrapped_record.respond_to?(:body)
          return @wrapped_record.body
        end

        return nil # Nil or unsupport object class. XXX donot put.
      end

    end # class Status
  end # module  Sync
end # module Mhc
