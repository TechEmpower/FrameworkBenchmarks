module Onyx::SQL
  class Query(T)
    # Add `RETURNING` clause either by explicit *values* or by a `T` fields.
    # When *values* contains a `Symbol`, it's treated as a `T` field and tried to be found in
    # **runtime**, raising otherwise. So you'll not know if you mistyped until the code is run.
    #
    # NOTE: You must refer a `T` field by its instance variable name, not the DB column name.
    # NOTE: All `RETURNING` clauses are removed on `Repository#exec(query)` call.
    # NOTE: SQLite does **not** support `RETURNING` clause.
    #
    # ```
    # q = user.insert.returning(:id, :name)
    # q.build # => {"INSERT INTO users ... RETURNING id, name"}
    #
    # q = user.insert.returning("foo")
    # q.build # => {"INSERT INTO users ... RETURNING foo"}
    # ```
    #
    # TODO: Make it type-safe.
    def returning(values : Enumerable(Symbol | Char | String))
      values.each do |value|
        {% begin %}
          {% table = T.annotation(Model::Options)[:table] %}

          case value
          {% for ivar in T.instance_vars.reject { |iv| (a = iv.annotation(Reference)) && a[:foreign_key] } %}
            when {{ivar.name.symbolize}}
              if @alias
                ensure_returning << ("#{@alias}.#{T.db_column({{ivar.name.symbolize}})}")
              else
                ensure_returning << ("{{table.id}}.#{T.db_column({{ivar.name.symbolize}})}")
              end
          {% end %}
          when Symbol
            raise ":#{value} symbol didn't match any of #{T} instance variables"
          else
            ensure_returning << value.to_s
          end
        {% end %}
      end

      self
    end

    # ditto
    def returning(*values : Symbol | Char | String)
      returning(values)
    end

    # Add `RETURNING` asterisk clause for the whole `T` table.
    #
    # NOTE: All `RETURNING` clauses are removed on `Repository#exec(query)` call.
    # NOTE: SQLite does **not** support `RETURNING` clause.
    #
    # ```
    # Post.returning(Post) # => RETURNING posts.*
    # ```
    def returning(klass : T.class)
      {% begin %}
        {% table = T.annotation(Model::Options)[:table] %}
        ensure_returning << "{{table.id}}.*"
      {% end %}

      self
    end

    @returning : Deque(String)? = nil
    protected property returning

    protected def get_returning
      @returning
    end

    protected def ensure_returning
      @returning ||= Deque(String).new
    end

    protected def append_returning(sql, *args)
      return if @returning.nil? || ensure_returning.empty?

      sql << " RETURNING "

      first = true
      ensure_returning.each do |value|
        sql << ", " unless first; first = false
        sql << value
      end
    end
  end
end
