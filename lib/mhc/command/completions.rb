module Mhc
  module Command
    class Completions

      def initialize(help, global_options, arguments, config = nil)
        @help, @global_options, @arguments, @config = help, global_options, arguments, config
        command_name = arguments.first

        if command_name and help[command_name]
          option_arguments(help, global_options, command_name)
        else
          command_arguments
        end
      end

      private

      def command_arguments
        print "_arguments\n"
        print "1:Possible commands\\::"
        print possible_commands + "\n"
      end

      def option_arguments(help, global_options, command_name)
        command_arguments
        print arguments(help, command_name)
        print options(help[command_name], global_options)
      end

      # make normal argument completion setting from usage string such as: "scan REPOSITORY"
      def arguments(help, command_name, position = 2)
        str = ""
        help[command_name].usage.split(/\s+/)[1..-1].each do |arg|
          pos = position

          if /^\[(.*)\]/ =~ arg
            arg = $1
          end

          multi = ""
          if /(.*)\.\.\.$/ =~ arg
            arg = $1
            pos = "*"
            multi = ":"
          end

          str << "#{pos}:#{arg}\\::#{possible_values(arg)}#{multi}\n"
          position += 1
        end
        return str
      end

      # make option argument completion setting from usage options help
      def options(command_help, global_options, position = 2)
        str = ""
        options = command_help.options.merge(global_options)

        options.each do |name, opt|
          if opt.type == :boolean
            str << "(--#{name})--#{name}[#{opt.description}]\n"
          else
            str << "(--#{name})--#{name}=-[#{opt.description}]:#{opt.banner}:#{possible_values_for_opt(opt)}\n"
          end
        end
        return str
      end

      def possible_commands
        str = "(("
        @help.each_value do |cmd|
          next if cmd.name == "completions"
          str << " #{cmd.name}\\:"
          str << cmd.description.gsub(/([()\s"';&|#\\])/, '\\\\\1')
        end
        str << "))"
      end

      def possible_values_for_opt(option)
        return "(" + option.enum.join(" ") + ")" if option.enum
        return possible_values(option.banner)
      end

      def possible_values(banner)
        case banner
        when /^CALENDAR/
          "(" + @config.calendars.select{|cal| cal.type == "mhc"}.map(&:name).join(' ') + ")"
        when /^(FILE|CONF)/
          "_files"
        when /^DIR/
          "_files -/"
        when "COMMAND"
          possible_commands
        when "RANGE"
          "(today tomorrow thismonth nextmonth)"
        when /^NUM/
          "_guard '[0-9]#' 'Number'"
        else
          ""
        end
      end

    end # class Completions
  end # module Command
end # module Mhc
