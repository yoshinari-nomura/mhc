#!/usr/bin/env ruby

require "net/https"
require "uri"
require "rexml/document"
require "fileutils"
require "pathname"

module Mhc
  class WebDav
    # WebDAV protocol: RFC4918
    # see http://tools.ietf.org/html/rfc4918
    # 
    class Client
      attr_reader :top_directory

      def initialize(base_url, proxy_host = nil, proxy_port = nil)
        uri = URI.parse(base_url)
        @top_directory = uri.path
        @http = Net::HTTP.new(uri.host, uri.port, proxy_host, proxy_port)
        @http.use_ssl = true if uri.scheme == "https"
        @http.verify_mode = OpenSSL::SSL::VERIFY_NONE
      end

      def set_basic_auth(user, password)
        @auth_user, @auth_password = user, password
        return self
      end

      # 8.1 PROPFIND
      def propfind(path = @top_directory, depth = 1, xml_body = nil)
        req = setup_request(Net::HTTP::Propfind, path)
        req['Depth'] = depth

        if xml_body
          req.content_type = 'application/xml; charset="utf-8"'
          req.content_length = xml_body.size
          req.body = xml_body
        end

        res = @http.request(req)

        if $MHC_DEBUG
          STDERR.print "\n* PROPFIND RESPONSE:\n"
          STDERR.print dump_response(res, true)
        end

        check_status_code(res, 207) # Multi-Status

        return res
      end

      # 8.2 PROPPATCH
      def proppatch
        raise NotImplementedError
      end

      # 8.3 MKCOL
      def mkcol(path)
        req = setup_request(Net::HTTP::Mkcol, path)
        res = @http.request(req)
        check_status_code(res, 201) # Created
        return res
      end

      # 8.4 GET
      def get(path)
        req = setup_request(Net::HTTP::Get, path)
        res = @http.request(req)
        check_status_code(res, 200) # OK
        return res
      end

      # 8.4 HEAD
      def head(path)
        req = setup_request(Net::HTTP::Head, path)
        res = @http.request(req)
        check_status_code(res, 200) # OK
        return res
      end

      # 8.5 POST 
      def post(content, dest_path)
        req = setup_request(Net::HTTP::Post, dest_path)
        req.content_length = content.size
        req.body = content
        res = @http.request(req)

        check_status_code(res, [201, 204]) # Created or No content
        return res
      end

      # 8.6 DELETE
      def delete(path, ifmatch = nil)
        req = setup_request(Net::HTTP::Delete, path)
        req['If-Match'] = ifmatch if ifmatch
        res = @http.request(req)

        check_status_code(res, 204)
        return res
      end

      # 8.7 PUT
      def put(content, dest_path, ifmatch = nil)
        req = setup_request(Net::HTTP::Put, dest_path)
        req.content_length = content.size
        req['If-Match'] = ifmatch if ifmatch
        req.content_type = "text/calendar; charset=utf-8" # xxx
        req.body = content
        res = @http.request(req)

        if $MHC_DEBUG
          STDERR.print "\n* PUT RESPONSE:\n"
          STDERR.print dump_response(res)
          STDERR.print "* HEAD RESPONSE:\n"
          STDERR.print dump_response(head(dest_path))
        end

        check_status_code(res, [201, 204]) # Created or No content
        return res
      end

      # 8.8 COPY
      def copy(src_path, dest_path)
        req = setup_request(Net::HTTP::Copy, src_path)
        req['Destination'] = dest_path

        res = @http.request(req)
        check_status_code(res, 204) # No Content
        return res
      end

      # 8.9 MOVE
      def move(src_path, dest_path)
        req = setup_request(Net::HTTP::Move, src_path)
        req['Destination'] = dest_path

        res = @http.request(req)
        check_status_code(res, 204) # No Content
        return res
      end

      # 8.10 LOCK
      def lock(path)
        raise NotImplementedError
      end

      # 8.11 UNLOCK
      def unlock(path)
        raise NotImplementedError
      end

      ################################################################
      private

      def check_status_code(res, required_status)
        unless ([required_status].flatten.map{|c| c.to_s}).member?(res.code)
          header = "Invalid HttpResponse"
          raise Exception.new("#{res.code} #{header} #{res.message} #{res.body}")
        end
      end

      def setup_request(request, *args)
        req = request.new(*args)
        req.basic_auth @auth_user, @auth_password

        # XXX: should implement re-connection mechanism for Keep-Alive:
        # http://d.hatena.ne.jp/daftbeats/20080321/1206092975
        req["Connection"] = "Keep-Alive"

        return req
      end

      def fetch(uri_str, limit = 10)
        raise StandardError, 'HTTP redirect too deep' if limit == 0

        response = Net::HTTP.get_response(URI.parse(uri_str))
        case response
        when Net::HTTPSuccess
          response
        when Net::HTTPRedirection
          fetch(response['location'], limit - 1)
        else
          response.value
        end
      end

      def dump_response(res, include_body = false)
        string = ""

        res.each do |name, value|
          string += "  #{name}: #{value}\n"
        end
        string += res.body + "\n" if include_body

        return string
      end

    end # class Client

    class Cache
      class DirectoryNotFoundError < StandardError
      end
      
      def initialize(top_directory)
        set_top_directory(top_directory)
      end

      def set_top_directory(path)
        raise DirectoryNotFoundError unless File.directory?(path)
        @local_top_pathname = Pathname.new(path)
        return self
      end

      def set_propfind_cache(path, xml)
        File.open(local_cache_path(path), "w") do |f|
          f.write(xml)
        end
      end

      def set_basic_auth(user, password)
        # nothing to do
        return self
      end

      # 8.1 PROPFIND
      def propfind(path, depth = 1, xml_body = nil)
        File.read(local_cache_path(path)) rescue nil
      end

      # 8.2 PROPPATCH
      def proppatch
        raise NotImplementedError
      end

      # 8.3 MKCOL
      def mkcol(path)
        File.mkdir(local_path(path))
      end

      # 8.4 GET
      def get(path)
        File.read(local_path(path))
      end

      # 8.4 HEAD
      def head(path)
        raise NotImplementedError
      end

      # 8.5 POST 
      def post(content, dest_path)
        raise NotImplementedError
      end

      # 8.6 DELETE
      def delete(path)
        File.unlink(local_path(path))
      end

      # 8.7 PUT
      def put(content, dest_path)
        make_directory_or_higher(File.dirname(local_path(dest_path)))

        File.open(local_path(dest_path), "w") do |f|
          f.write(content)
        end
      end

      # 8.8 COPY
      def copy(src_path, dest_path)
        raise NotImplementedError
      end

      # 8.9 MOVE
      def move(src_path, dest_path)
        raise NotImplementedError
      end

      # 8.10 LOCK
      def lock(path)
        raise NotImplementedError
      end

      # 8.11 UNLOCK
      def unlock(path)
        raise NotImplementedError
      end

      private

      def make_directory_or_higher(directory)
        unless File.directory?(directory)
          parent = File.dirname(directory)
          make_directory_or_higher(parent)
          print "mkdir #{directory}\n"
          return Dir.mkdir(directory)
        end
      end

      def local_pathname(path)
        pathname = Pathname.new(path)
        raise "path (#{path.to_s})should be absolute." unless pathname.absolute?
        (@local_top_pathname + ("./" + pathname)).cleanpath
      end

      def local_path(path)
        local_pathname(path).to_s
      end

      def local_cache_path(path)
        if File.directory?(local_path(path))
          (local_pathname(path) + "propfind-cache.xml").cleanpath.to_s
        else
          local_path(path)
        end
      end
    end # class Cache
  end # class WebDav
end # module Mhc
