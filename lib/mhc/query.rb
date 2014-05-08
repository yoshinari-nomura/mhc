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
        return lambda {|ev| !!@terms.find {|term| term.to_proc.call(ev)}}
      end
    end

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
    end

    #
    # Factor :: '!'* ( '(' Expression ')' || KeyValuePair )
    #
    class Factor
      def initialize(context)
        @expected_value = true
        @expected_value = !@expected_value while context.eat_if(:negop)

        if context.eat_if(:lparen)
          @value = Expression.new(context)
          context.expect(:rparen)
        else
          @value = KeyValuePair.new(context)
        end
      end

      def to_proc
        @proc = @value.to_proc
        return lambda {|ev| @proc.call(ev) == @expected_value}
      end
    end

    #
    # KeyValuePair :: Symbol ':' (Symbol || String)
    #
    # String should be surrounded by double quote ``"''
    #
    class KeyValuePair
      KEYWORDS = [:subject, :category, :body, :location]
      def initialize(context)
        @name = context.expect(:symbol).value.downcase.to_sym
        context.expect(:sepop)
        if token = context.eat_if(:symbol)
          @value = token.value
        else
          @value = context.expect(:string).value[1..-2]
        end
        raise ParseError, "unknown keyword '#{@name}'" unless KEYWORDS.member?(@name)
      end

      def to_proc
        case @name
        when :category
          return lambda {|ev| ev.categories.map{|c| c.to_s.downcase}.include?(@value.downcase)}
        else
          return lambda {|ev| !!ev.send(@name).to_s.toutf8.match(Regexp.quote(@value))}
        end
      end
    end

    class Context
      TOKENS = {
        symbol: /[a-zA-Z_][a-zA-Z_\d]*/,
        string: /"(?:[^"\\]|\\.)*"/,
        negop:  /!/,
        andop:  /&/,
        orop:   /\|/,
        sepop:  /:/,
        lparen: /\(/,
        rparen: /\)/
      }.map{|type,regexp| "(?<#{type}>#{regexp})"}.join("|")

      TOKEN_REGEXP = Regexp.new('^\s*(' + TOKENS + ')')

      def initialize(string)
        @tokens = tokenize(string)
      end

      def eat_if(expected_type)
        if @tokens.first and @tokens.first.type == expected_type
          return @tokens.shift
        else
          return false
        end
      end

      def expect(expected_type)
        token = eat_if(expected_type) and return token
        raise ParseError, "#{expected_type.upcase} expected before #{@tokens.first.value rescue 'END'}"
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
    end

    class Token
      attr_reader :type, :value

      def initialize(type, string)
        @type, @value = type.to_sym, string
      end
    end

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
