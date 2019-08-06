require "fileutils"

module Mhc
  module Command
    class Init
      SUB_DIRS = %w(draft inbox presets spool trash
               status/cache status/log status/sync_channels)

      TEMPLATE_DIR = File.expand_path("../../templates", __FILE__)

      def initialize(top_dir, config_path, tzid = nil, template_dir = nil)
        @shell = Thor.new
        @status = {green: 0, yellow: 0, red: 0}
        @config = {}

        # guess teimzone
        say "Guessing current local timezone ..."

        if @config[:tzid] = find_current_tzid
          say_status "ok", "guess timezone ... #{@config[:tzid]}", :green
          tzid = find_current_tzid
        else
          say_status "failed", "guess timezone... Unknown", :red
        end

        # mkdir
        say "Making directries under #{top_dir} ..."
        SUB_DIRS.each do |ent|
          mkdir_p(File.expand_path(ent, top_dir))
        end

        # make config file from tamplate
        say "Copying config file(s) into #{config_path} ..."
        src = File.expand_path("config.yml.erb", TEMPLATE_DIR)
        dst = File.expand_path(config_path)
        @config[:topdir] = top_dir
        expand_template(src, dst)

        say_status_report
      end

      private

      def say(message, color = nil)
        @shell.say(message, color)
      end

      def say_status(status, message, log_status = nil)
        @status[log_status] += 1
        @shell.say_status(status, message, log_status)
      end

      def say_status_report
        if (errors = @status[:red]) > 0
          say "#{errors} error(s) were occurred.", :red
        else
          say "Done."
        end
      end

      def expand_template(template_path, dest_path)
        require "erb"
        template = ERB.new(File.open(template_path).read, nil, "-")

        if File.exists?(dest_path)
          say_status "exist", "Ignore #{dest_path}", :yellow
          return
        end

        begin
          mkdir_p(File.expand_path("..", dest_path))
          File.open(dest_path, "w", 0600) do |file|
            file.write(template.result(binding))
          end
          say_status "ok", "copy #{dest_path}", :green
        rescue StandardError => e
          say_status "failed", "#{e.message.split(' @').first} #{dest_path}", :red
        end
      end

      def mkdir_p(path)
        path = File.expand_path(path)

        if File.directory?(path)
          say_status "exist", "Ignore #{path}", :yellow
          return
        end

        begin
          FileUtils.mkdir_p(path)
          say_status "create", "#{path}", :green
        rescue StandardError => e
          say_status "failed", "#{e.message.split(' @').first} #{path}", :red
        end
      end

      def find_current_tzid
        require "digest/md5"

        # Debian
        if File.exists?("/etc/timezone")
          return File.open("/etc/timezone").read.chomp
        end

        # Mac
        if File.symlink?("/etc/localtime") &&
            /([^\/]+\/[^\/]+)$/ =~ File.readlink("/etc/localtime")
          return $1
        end

        # Red Had / CentOS
        if File.exists?("/etc/sysconfig/clock") &&
            /ZONE=["']?([^"']+)/ =~ File.open("/etc/sysconfig/clock").read.chomp
          return $1
        end

        # generic including FreeBSD
        if File.exists?("/etc/localtime")
          localtime = Digest::MD5.file("/etc/localtime")
          candidates = Dir.chdir("/usr/share/zoneinfo") do
            Dir.glob("**/*").select do |fn|
              File.file?(fn) && Digest::MD5.file(fn) == localtime
            end
          end
          unless candidates.empty?
            # take the most descriptive (has long name) one
            return candidates.sort {|a,b| b.length <=> a.length}.first
          end
        end
        return "Unknown"
      end

    end # class Init
  end # module Command
end # module Mhc
