module Mhc
  class Query
    def initialize(query_string)
      @expression = Expression.new(Context.new(query_string))
      @query_string = query_string
    end

    def to_proc
      return @expression.to_proc
    end

    def to_s
      @query_string.to_s
    end

    class ParseError < StandardError; end

    #
    # Expression :: Term ('|' Term)*
    #
    class Expression
      def initialize(context)
        @terms = [Term.new(context)]
        @terms << Term.new(context) while context.eat_if(:orop)
      end

      def to_proc
        @procs = @terms.map(&:to_proc)
        return lambda {|ev| @procs.any? {|p| p.call(ev)}}
      end
    end # class Expression

    #
    # Term :: Factor ('&' Factor)*
    #
    class Term
      def initialize(context)
        @factors = [Factor.new(context)]
        @factors << Factor.new(context) while context.eat_if(:andop)
      end

      def to_proc
        @procs = @factors.map(&:to_proc)
        return lambda {|ev| @procs.all? {|p| p.call(ev)}}
      end
    end # class Term

    #
    # Factor :: '!'* ( '(' Expression ')' || RelationalExpression )
    #
    class Factor
      def initialize(context)
        @expected_value = true
        @expected_value = !@expected_value while context.eat_if(:negop)

        if context.eat_if(:lparen)
          @value = Expression.new(context)
          context.expect(:rparen)
        else
          @value = RelationalExpression.new(context)
        end
      end

      def to_proc
        @proc = @value.to_proc
        return lambda {|ev| @proc.call(ev) == @expected_value}
      end
    end # class Factor

    #
    # RelationalExpression :: Symbol Operator (Argument || '[' Argument Argument* ']')
    #
    class RelationalExpression
      KEYWORDS = [:subject, :category, :body, :location, :recurrence_tag]

      def initialize(context)
        @name = context.expect(:symbol).value.downcase.to_sym
        raise ParseError, "unknown keyword '#{@name}'" unless KEYWORDS.member?(@name)

        context.expect(:sepop) # Currently, operator is only ":"

        @arguments = []
        if context.eat_if(:lbracket)
          loop do
            @arguments << Argument.new(context)
            break if context.eat_if(:rbracket)
          end
        else
          @arguments << Argument.new(context)
        end
      end

      def to_proc
        case @name
        when :category
          @arguments = @arguments.map{|arg| arg.value.downcase}
          return lambda {|ev| !(ev.categories.map{|c| c.to_s.downcase} & @arguments).empty?}
        when :recurrence_tag
          @arguments = @arguments.map{|arg| arg.value.downcase}
          return lambda {|ev| !!@arguments.find{|v| ev.send(@name).to_s.downcase.toutf8 == v}}
        else
          @arguments = @arguments.map{|arg| Regexp.quote(arg.value)}
          return lambda {|ev| !!@arguments.find{|v| ev.send(@name).to_s.toutf8.match(v)}}
        end
      end
    end # class RelationalExpression

    #
    # Argument :: Symbol || String
    #
    class Argument
      def initialize(context)
        token = context.expect(:symbol, :string)
        @type  = token.type
        @value = token.value
      end

      def value
        case @type
        when :string
          @value[1..-2]
        else
          @value
        end
      end
    end # class Argument

    class Context
      TOKENS = {
        symbol:   /[a-zA-Z_][a-zA-Z_\d]*/,
        string:   /"(?:[^"\\]|\\.)*"/,
        negop:    /!/,
        andop:    /&/,
        orop:     /\|/,
        sepop:    /:/,
        lparen:   /\(/,
        rparen:   /\)/,
        lbracket: /\[/,
        rbracket: /\]/
      }.map{|type,regexp| "(?<#{type}>#{regexp})"}.join("|")

      TOKEN_REGEXP = Regexp.new('^\s*(' + TOKENS + ')')

      def initialize(string)
        @tokens = tokenize(string)
      end

      def eat_if(*expected_types)
        expected_types.each do |expected_type|
          if @tokens.first and @tokens.first.type == expected_type
            return @tokens.shift
          end
        end
        return nil
      end

      def expect(*expected_types)
        token = eat_if(*expected_types) and return token
        raise ParseError, "#{expected_types.map(&:upcase).join(' or ')} expected before #{@tokens.first.value rescue 'END'}"
      end

      def debug_dump
        @tokens.map{|token| "#{token.type} => #{token.value}"}.join(", ")
      end

      private

      def tokenize(string)
        tokens = []

        loop do
          token, string = get_token(string)
          break if token.nil?
          tokens << token
        end

        raise ParseError, "can not tokenize '#{string}'" unless string.length == 0
        return tokens
      end

      def get_token(string)
        if match = TOKEN_REGEXP.match(string)
          name   = match.names.find{|name| match[name]}
          value  = match[name]
          remain = match.post_match.strip
          return [Token.new(name, value), remain]
        end
        return [nil, string]
      end
    end # class Context

    class Token
      attr_reader :type, :value

      def initialize(type, string)
        @type, @value = type.to_sym, string
      end
    end # class Token

    class Test
      attr_reader :categories, :subject

      def initialize(categories = [], subject = "", body = "")
        @categories = categories
        @subject = subject
        @body = body
      end
    end
  end
end
