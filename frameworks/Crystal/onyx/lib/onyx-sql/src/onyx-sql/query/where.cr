module Onyx::SQL
  class Query(T)
    # Add `WHERE` clause with *values*. All clauses in a single call are concatenated with `AND`.
    # It's a **type-safe** method.
    #
    # ```
    # query = User.where(name: "John", age: 18)
    # query.build # => {"SELECT ... FROM users WHERE (name = ? AND age = ?)", {"John", 18}}
    # ```
    def where(or : Bool = false, not : Bool = false, **values : **U) : self forall U
      {% begin %}
        internal_clauses = uninitialized String[{{U.size}}]
        internal_params = Deque(DB::Any).new

        {% table = T.annotation(Model::Options)[:table].id %}

        values.each_with_index do |key, value, index|
          case key
          {% for key, value in U %}
            {% found = false %}

            {% for ivar in T.instance_vars %}
              # If the ivar is a reference
              {% if ann = ivar.annotation(Reference) %}
                {% if key == ivar.name || key == ann[:key].id %}
                  {%
                    type = ivar.type.union_types.find { |t| t != Nil }
                    pk = type.annotation(Model::Options)[:primary_key]
                  %}

                  # Iterate through reference instance vars to find the primary key var
                  #

                  {% pk_rivar = nil %}

                  {% for rivar in type.instance_vars %}
                    {% if "@#{rivar.name}".id == pk.name.id %}
                      {% pk_rivar = rivar %}
                    {% end %}
                  {% end %}

                  {% raise "`primary_key: #{pk}` option didn't match any of `#{type}` instance variables" unless pk_rivar %}

                  {% reference_sql_key = ivar.annotation(Reference)[:key].id %}

                  # If the key is a reference primary key (e.g. `#where(author_id: 42)`)
                  {% if key.id == ann[:key].id %}
                    {% raise "Invalid compile-time type `#{value}` for argument `#{key.symbolize}` in `Query(#{T})#where` call. Expected: `#{pk_rivar.type}`" unless value <= pk_rivar.type %}

                    {% found = true %}

                    when {{key.symbolize}}
                      if value.nil?
                        internal_clauses[index] = (@alias || {{table.stringify}}) + ".{{reference_sql_key}} IS NULL"
                      else
                        internal_clauses[index] = (@alias || {{table.stringify}}) + ".{{reference_sql_key}} = ?"
                        internal_params << {{type}}.db_values({{pk_rivar.name}}: value.as({{value}}))[0]
                      end

                  # If the key is a reference name (e.g. `#where(author: user)`)
                  {% elsif key.id == ivar.name %}
                    {% raise "Invalid compile-time type `#{value}` for argument `#{key.symbolize}` in `Query(#{T})#where` call. Expected: `#{ivar.type}`" unless value <= ivar.type %}

                    {% found = true %}

                    when {{key.symbolize}}
                      if value.nil?
                        internal_clauses[index] = (@alias || {{table.stringify}}) + ".{{reference_sql_key}} IS NULL"
                      else
                        internal_clauses[index] = (@alias || {{table.stringify}}) + ".{{reference_sql_key}} = ?"
                        internal_params << {{type}}.db_values({{pk_rivar.name}}: value.as({{value}}).{{pk_rivar.name}})[0]
                      end
                  {% end %}
                {% elsif key == ann[:foreign_key].id %}
                  {% raise "Cannot call `Query(#{T})#where` with foreign reference key argument `#{key.symbolize}`" %}
                {% end %}

              # If the ivar is not a reference, but a field
              {% elsif key.id == ivar.name %}
                {% raise "Invalid compile-time type `#{value}` for argument `#{key.symbolize}` in `Query(#{T})#where` call. Expected: `#{ivar.type}`" unless value <= ivar.type %}

                {% found = true %}

                {% field_sql_key = ((a = ivar.annotation(Field)) && (k = a[:key]) && k.id) || ivar.name %}

                when {{key.symbolize}}
                  if value.nil?
                    internal_clauses[index] = (@alias || {{table.stringify}}) + ".{{field_sql_key}} IS NULL"
                  else
                    internal_clauses[index] = (@alias || {{table.stringify}}) + ".{{field_sql_key}} = ?"
                    internal_params << T.db_values({{ivar.name}}: value.as({{value}}))[0]
                  end
              {% end %}
            {% end %}

            {% raise "Class `#{T}` has neither field nor reference with key `#{key.symbolize}` eligible for `Query(#{T})#where` call" unless found %}
          {% end %}
          end
        end

        ensure_where << Where.new(
          clause: internal_clauses.join(" AND "),
          params: internal_params,
          or: or,
          not: not
        )

        @latest_wherish_clause = :where
      {% end %}

      self
    end

    # Add `NOT` clause with *values* to `WHERE`.
    #
    # ```
    # query.where_not(id: 42) # => "WHERE (...) AND NOT (id = ?)"
    # ```
    def where_not(**values)
      where(**values, not: true)
    end

    {% for or in [true, false] %}
      {% for not in [true, false] %}
        # Add `{{or ? "OR".id : "AND".id}}{{" NOT".id if not}}` clause with *values* to `WHERE`.
        #
        # ```
        # {{(or ? "or" : "and").id}}_where{{"_not".id if not}}(id: 42) # => "WHERE (...) {{or ? "OR ".id : "AND ".id}}{{"NOT ".id if not}}(id = ?)"
        # ```
        def {{(or ? "or" : "and").id}}_where{{"_not".id if not}}(**values : **U) forall U
          where(**values, or: {{or}}, not: {{not}})
        end

        # This method will raise in compilation-time,
        # because having a `HAVING` query with a `Model`'s attributes makes no sense.
        def {{(or ? "or" : "and").id}}_having{{"_not".id if not}}(**values : **U) forall U
          \{% begin %}
            \{% pretty_values = "" %}

            \{% for key, value, index in U %}
              \{% pretty_values = pretty_values + "#{key}: #{value}" %}
              \{% pretty_values = pretty_values + ", " if index < (U.size - 1) %}
            \{% end %}

            \{% raise "Cannot call `Query(#{T})##{ {{(or ? "or" : "and")}} }_having#{ {{not ? "_not" : ""}} }(#{pretty_values.id})` because `HAVING` clause with direct `#{T}` fields or references makes no sense. Use the string clause version instead" %}
          \{% end %}
        end
      {% end %}
    {% end %}
  end
end
