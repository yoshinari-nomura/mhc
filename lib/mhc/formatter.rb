module Mhc
  class Formatter

    class NameError < StandardError; end

    def self.build(formatter:, date_range:, **options)
      case formatter.to_sym
      when :text
        Text.new(date_range: date_range, options:options)
      when :mail
        Mail.new(date_range: date_range, options:options)
      when :orgtable
        OrgTable.new(date_range: date_range, options:options)
      when :emacs
        Emacs.new(date_range: date_range, options:options)
      when :icalendar
        Icalendar.new(date_range: date_range, options:options)
      when :calfw
        SymbolicExpression.new(date_range: date_range, options:options)
      when :howm
        Howm.new(date_range: date_range, options:options)
      when :json
        Json.new(date_range: date_range, options:options)
      else
        raise Formatter::NameError.new("Unknown format: #{formatter} (#{formatter.class})")
      end
    end

    dir = File.dirname(__FILE__) + "/formatter"

    autoload :Base,               "#{dir}/base.rb"
    autoload :Emacs,              "#{dir}/emacs.rb"
    autoload :Howm,               "#{dir}/howm.rb"
    autoload :Icalendar,          "#{dir}/icalendar.rb"
    autoload :Json,               "#{dir}/json.rb"
    autoload :Mail,               "#{dir}/mail.rb"
    autoload :OrgTable,           "#{dir}/org_table.rb"
    autoload :SymbolicExpression, "#{dir}/symbolic_expression.rb"
    autoload :Text,               "#{dir}/text.rb"

  end # class Formatter
end # module Mhc
