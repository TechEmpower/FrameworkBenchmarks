module Onyx::SQL
  class Query(T)
    {% for wherish in %w(where having) %}
      # Add `{{wherish.upcase.id}}` *clause* with *params*.
      #
      # ```
      # query = User.{{wherish.id}}("foo = ?", "bar")
      # query.build # => {"{{wherish.upcase.id}} (foo = ?)", {"bar"}}
      # ```
      #
      # Multiple calls concatenate clauses with `AND`:
      #
      # ```
      # query = User.{{wherish.id}}("foo = ?", "bar").{{wherish.id}}("baz = ?", 42)
      # query.build # => {"{{wherish.upcase.id}} (foo = ?) AND (baz = ?)", {"bar", 42}}
      # ```
      def {{wherish.id}}(clause : String, *params : DB::Any, or : Bool = false, not : Bool = false)
        ensure_{{wherish.id}} << {{wherish.camelcase.id}}.new(
          clause: clause,
          params: params.to_a.map(&.as(DB::Any)),
          or: or,
          not: not
        )

        @latest_wherish_clause = :{{wherish.id}}

        self
      end

      # Add `{{wherish.upcase.id}}` *clause* without params.
      #
      # ```
      # query = User.{{wherish.id}}("foo IS NULL")
      # query.build # => {"{{wherish.upcase.id}} (foo IS NULL)", {}}
      # ```
      #
      # Multiple calls concatenate clauses with `AND`:
      #
      # ```
      # query = User.{{wherish.id}}("foo IS NULL").{{wherish.id}}("bar IS NOT NULL")
      # query.build # => {"{{wherish.upcase.id}} (foo IS NULL) AND (bar IS NOT NULL)", {}}
      # ```
      def {{wherish.id}}(clause : String, or : Bool = false, not : Bool = false)
        ensure_{{wherish.id}} << {{wherish.camelcase.id}}.new(
          clause: clause,
          params: nil,
          or: or,
          not: not
        )

        @latest_wherish_clause = :{{wherish.id}}

        self
      end

      # Add `NOT` *clause* with *params* to `{{wherish.upcase.id}}`.
      #
      # ```
      # query.{{wherish.id}}_not("foo = ?", "bar") # => "{{wherish.upcase.id}} (...) AND NOT (foo = ?)"
      # ```
      def {{wherish.id}}_not(clause, *params)
        {{wherish.id}}(clause, *params, not: true)
      end

      # Add `NOT` *clause* to `{{wherish.upcase.id}}`.
      #
      # ```
      # query.{{wherish.id}}_not("foo IS NULL") # => "{{wherish.upcase.id}} (...) AND NOT (foo IS NULL)"
      # ```
      def {{wherish.id}}_not(clause)
        {{wherish.id}}(clause, not: true)
      end

      {% for or in [true, false] %}
        {% for not in [true, false] %}
          # Add `{{or ? "OR".id : "AND".id}}{{" NOT".id if not}}` *clause* with *params* to `{{wherish.upcase.id}}`.
          #
          # ```
          # query.{{(or ? "or" : "and").id}}_{{wherish.id}}{{"_not".id if not}}("foo = ?", "bar") # => "{{wherish.upcase.id}} (...) {{or ? "OR ".id : "AND ".id}}{{"NOT ".id if not}}(foo = ?)"
          # ```
          def {{(or ? "or" : "and").id}}_{{wherish.id}}{{"_not".id if not}}(clause : String, *params)
            {{wherish.id}}(clause, *params, or: {{or}}, not: {{not}})
          end

          # Add `{{or ? "OR".id : "AND".id}}{{" NOT".id if not}}` *clause* to `{{wherish.upcase.id}}`.
          #
          # ```
          # query.{{(or ? "or" : "and").id}}_{{wherish.id}}{{"_not".id if not}}("foo IS NULL") # => "{{wherish.upcase.id}} (...) {{or ? "OR ".id : "AND ".id}}{{"NOT ".id if not}}(foo IS NULL)"
          # ```
          def {{(or ? "or" : "and").id}}_{{wherish.id}}{{"_not".id if not}}(clause : String)
            {{wherish.id}}(clause, or: {{or}}, not: {{not}})
          end
        {% end %}
      {% end %}

      private struct {{wherish.camelcase.id}}
        getter clause, params, or, not

        def initialize(
          @clause : String,
          @params : Enumerable(DB::Any)?,
          @or : Bool,
          @not : Bool
        )
        end
      end

      @{{wherish.id}} : Deque({{wherish.camelcase.id}}) | Nil = nil

      protected def get_{{wherish.id}}
        @{{wherish.id}}
      end

      protected def ensure_{{wherish.id}}
        @{{wherish.id}} = Deque({{wherish.camelcase.id}}).new if @{{wherish.id}}.nil?
        @{{wherish.id}}.not_nil!
      end

      protected def append_{{wherish.id}}(sql, params, params_index)
        if {{wherish.id}} = @{{wherish.id}}
          sql << " {{wherish.upcase.id}} "
          first_clause = true

          {{wherish.id}}.each do |clause|
            if pi = params_index
              formatted_clause = clause.clause.gsub("?") { "$#{pi.value += 1}" }
            else
              formatted_clause = clause.clause
            end

            sql << (clause.or ? " OR " : " AND ") unless first_clause
            sql << "NOT " if clause.not
            sql << "(" << formatted_clause << ")"

            first_clause = false

            unless params.nil? || clause.params.nil?
              clause.params.not_nil!.each do |param|
                params.not_nil! << param
              end
            end
          end
        end
      end
    {% end %}
  end
end
