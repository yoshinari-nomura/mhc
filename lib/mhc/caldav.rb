require File.dirname(__FILE__) + '/webdav'

module Net
  class HTTP
    class Report < HTTPRequest
      METHOD = 'REPORT'
      REQUEST_HAS_BODY  = true
      RESPONSE_HAS_BODY = true
    end
  end
end

module Mhc
  class CalDav
    class Report
      attr_accessor :uid, :etag, :href, :content_type, :status, :ics

      def self.parse(xmldoc)
        info = self.new

        href, status, content_type, etag, ics =
          %w(D:href
             D:propstat/D:status
             D:propstat/D:prop/D:getcontenttype
             D:propstat/D:prop/D:getetag
             D:propstat/D:prop/caldav:calendar-data
          ).map{|e| xmldoc.elements[e].text rescue nil}

        info.href = URI.unescape(href)
        info.uid = File.basename(info.href, ".ics")
        info.status = status
        info.content_type = content_type
        info.etag = etag # unquote_string(etag)
        info.ics = ics
        return info
      end

      private_class_method

      def self.unquote_string(str)
        return str.gsub('"', "")
      end

    end # class Report

    class CalendarProperty
      attr_accessor :description, :color, :displayname, :ctag

      def initialize(description, color, displayname, ctag)
        @description, @color, @displayname, @ctag = description, color, displayname, ctag
      end

      def self.parse(xml)
        xml = REXML::Document.new(xml) if xml.is_a?(String)
        description, color, displayname, ctag =
          %w(caldav:calendar-description
             ical:calendar-color
             D:displayname
             cs:getctag
          ).map{|e| xml.elements[e].text rescue nil}
        self.new(description, color, displayname, ctag)
      end
    end

    class ReportCollection
      def initialize
        @db = {}
        @calendar_property = nil
      end

      def collection
        return @db
      end

      def find(uid)
        return @db[uid]
      end

      def uid_list
        return @db.keys
      end

      def update(info)
        @db[info.uid] = info
      end

      def calendar_property
        @calendar_property
      end

      def set_calendar_property(xml)
        @calendar_property = CalendarProperty.parse(xml)
      end

      def self.parse(xml)
        db = self.new
        xml = REXML::Document.new(xml) if xml.is_a?(String)

        xml.elements.each("D:multistatus/D:response") do |res|
          if res.elements["D:propstat/D:prop/D:resourcetype/D:collection"]
            db.set_calendar_property(res.elements["D:propstat/D:prop"])
          else
            db.update(Report.parse(res))
          end
        end
        return db
      end
    end # class ReportCollection


    class Client < WebDav::Client

      def report(xml, path = @top_directory, depth = 1)
        req = setup_request(Net::HTTP::Report, path)
        req['Depth'] = depth
        req.content_length = xml.size
        req.content_type = 'application/xml; charset="utf-8"'
        req.body = xml
        res = @http.request(req)
        #check_status_code(res, 207)
        return res
      end

      ## for caldav sync
      ## etag_report is one of:
      ##   + {uid_string => etag_object } style hash,
      ##   + {uid_string => etag_string } style hash,
      ##   + [uid_etag_object] style array,
      ## uid_etag_object is an object which respond to #etag and #uid method.
      ## etag_object is an object which respond to #etag method.
      def report_etags(uids = nil) # XXX: handle uids
        ReportCollection.parse(self.propfind.body).collection
      end

      # for caldav sync
      def get_with_etag(uid_or_href)
        res = get(uid_or_href)
        return [res, res['etag']]
      end

      # for caldav sync
      # return value : false  ... failed.
      # return value : true   ... successful but etag is not available
      # return value : String ... successful with new etag
      def put_if_match(uid, ics_string, etag)
        STDERR.print "CALDAV put_if_match(:uid => #{uid}, :etag => #{etag} ..."
        begin
        res = put(ics_string, uid, etag)
        rescue Exception => e
          STDERR.print "failed: (#{e.to_s})\n"
          return false
        end
        STDERR.print "succeeded: #{res}\n"
        return res['etag'] || true
      end

      def delete_if_match(uid, etag)
        begin
          res = delete(uid, etag)
        rescue Exception => e
          return false
        end
        return true
      end

      def delete(uid_or_href, ifmatch = nil)
        super(adjust_path(uid_or_href))
      end

      def get(uid_or_href)
        super(adjust_path(uid_or_href))
      end

      def head(uid_or_href)
        super(adjust_path(uid_or_href))
      end

      def put(content, uid_or_href, ifmatch = nil)
        super(content, adjust_path(uid_or_href), ifmatch)
      end

      def report_calendar_multiget(href_list, path = @top_directory)
        xml = '<?xml version="1.0" encoding="utf-8" ?>'
        xml += <<-EOS
          <C:calendar-multiget xmlns:D="DAV:"
             xmlns:C="urn:ietf:params:xml:ns:caldav">
            <D:prop>
              <D:getetag/>
              <C:calendar-data/>
            </D:prop>
            #{href_list.map{|href| "<D:href>" + href + "</D:href>\n"}}
          </C:calendar-multiget>
        EOS
        return ReportCollection.parse(report(xml, path).body)
      end

      def fetch_calendar_list(url, username, userpass)
        depth = 1
        split_url = URI.split(url)
        host_url = split_url[0] + "://" + split_url[2]
        body = <<-EOF_BODY
          <?xml version="1.0" encoding="UTF-8"?>
           <A:propfind xmlns:A="DAV:">
            <A:prop>
             <A:add-member/>
             <C:allowed-sharing-modes xmlns:C="http://calendarserver.org/ns/"/>
             <D:autoprovisioned xmlns:D="http://apple.com/ns/ical/"/>
             <E:bulk-requests xmlns:E="http://me.com/_namespace/"/>
             <D:calendar-color xmlns:D="http://apple.com/ns/ical/"/>
             <B:calendar-description xmlns:B="urn:ietf:params:xml:ns:caldav"/>
             <B:calendar-free-busy-set xmlns:B="urn:ietf:params:xml:ns:caldav"/>
             <D:calendar-order xmlns:D="http://apple.com/ns/ical/"/>
             <B:calendar-timezone xmlns:B="urn:ietf:params:xml:ns:caldav"/>
             <A:current-user-privilege-set/>
             <B:default-alarm-vevent-date xmlns:B="urn:ietf:params:xml:ns:caldav"/>
             <B:default-alarm-vevent-datetime xmlns:B="urn:ietf:params:xml:ns:caldav"/>
             <A:displayname/>
             <C:getctag xmlns:C="http://calendarserver.org/ns/"/>
             <D:language-code xmlns:D="http://apple.com/ns/ical/"/>
             <D:location-code xmlns:D="http://apple.com/ns/ical/"/>
             <A:owner/>
             <C:pre-publish-url xmlns:C="http://calendarserver.org/ns/"/>
             <C:publish-url xmlns:C="http://calendarserver.org/ns/"/>
             <C:push-transports xmlns:C="http://calendarserver.org/ns/"/>
             <C:pushkey xmlns:C="http://calendarserver.org/ns/"/>
             <A:quota-available-bytes/>
             <A:quota-used-bytes/>
             <D:refreshrate xmlns:D="http://apple.com/ns/ical/"/>
             <A:resource-id/>
             <A:resourcetype/>
             <B:schedule-calendar-transp xmlns:B="urn:ietf:params:xml:ns:caldav"/>
             <B:schedule-default-calendar-URL xmlns:B="urn:ietf:params:xml:ns:caldav"/>
             <C:source xmlns:C="http://calendarserver.org/ns/"/>
             <C:subscribed-strip-alarms xmlns:C="http://calendarserver.org/ns/"/>
             <C:subscribed-strip-attachments xmlns:C="http://calendarserver.org/ns/"/>
             <C:subscribed-strip-todos xmlns:C="http://calendarserver.org/ns/"/>
             <B:supported-calendar-component-set xmlns:B="urn:ietf:params:xml:ns:caldav"/>
             <B:supported-calendar-component-sets xmlns:B="urn:ietf:params:xml:ns:caldav"/>
             <A:supported-report-set/>
             <A:sync-token/>
            </A:prop>
           </A:propfind>
          EOF_BODY
        res = self.propfind(url, depth, body)
        return [] if (res.code.to_i / 200) != 1

        xml = Nokogiri::XML(res.body).remove_namespaces!
        blocks = xml.xpath('//multistatus/response')

        calendars = []

        blocks.each do |block|
          if block.xpath('propstat/prop/calendar-color')[0].content != ""
            href = block.xpath('href')[0].content
            displayname = block.xpath('propstat/prop/displayname')[0].content
            color = block.xpath('propstat/prop/calendar-color')[0].content
            if color =~ /^#(..)(..)(..)/
              color = "#" + $1 + $2 + $3
              # color = double_lightness_of_hexrgb(color)
            end
            description = block.xpath('propstat/prop/calendar-description')[0].content
            calendars << {"url" => host_url + href.to_s, "displayname" => displayname.to_s,
              "color" => color.to_s, "description" => description.to_s }
          end
        end

        return calendars
      end

      private
      def adjust_path(uid_or_href) # XXX: google calendar specific?
        if uid_or_href =~ /^\//
          return uid_or_href
        else
          return File.expand_path(uid_or_href, @top_directory)
        end
      end

    end # class Client

    class Cache < WebDav::Cache
      def report(xml, depth = 1)
        raise NotImplementedError
      end

      def report_calendar_multiget(path_list)
        raise NotImplementedError
      end
    end # class Cache
  end
end
