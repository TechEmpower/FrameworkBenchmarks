module Onyx::SQL
  class Query(T)
    # Add `SELECT` clause either by explicit *values* or by a `T` fields.
    # When *values* contains a `Symbol`, it's treated as a `T` field and tried to be found in
    # **runtime**, raising otherwise. So you'll not know if you mistyped until the code is run.
    #
    # If no `#select` is called on a query, then it would select the whole table (`"table.*"`).
    #
    # NOTE: You must refer a `T` field by its instance variable name, not the DB column name.
    #
    # ```
    # q = User.all
    # q.build # => {"SELECT users.* FROM users"}
    #
    # q = User.select(:id, :name)
    # q.build # => {"SELECT users.id, users.name FROM users"}
    #
    # q = User.select("foo")
    # q.build # => {"SELECT foo FROM users"}
    # ```
    #
    # TODO: Make it type-safe.
    def select(values : Enumerable(Symbol | String))
      values.each do |value|
        {% begin %}
          {% table = T.annotation(Model::Options)[:table] %}

          case value
          {% for ivar in T.instance_vars.reject { |iv| (a = iv.annotation(Reference)) && a[:foreign_key] } %}
            {% key = ((a = ivar.annotation(Field)) && a[:key]) || ivar.name %}

            when {{ivar.name.symbolize}}
              if @alias
                ensure_select << ("#{@alias}.#{T.db_column({{ivar.name.symbolize}})}")
              else
                ensure_select << ("{{table.id}}.#{T.db_column({{ivar.name.symbolize}})}")
              end
          {% end %}
          when Symbol then raise "No instance variables found by symbol :#{value} in #{T}"
          else
            ensure_select << value
          end
        {% end %}
      end

      @type = :select
      self
    end

    # ditto
    def select(*values : Symbol | String)
      self.select(values)
    end

    # Add `SELECT` asterisk clause for the whole `T` table.
    #
    # ```
    # Post.select(Post, :id) # => SELECT posts.*, posts.id
    # ```
    def select(klass : T.class, *args)
      {% begin %}
        {% table = T.annotation(Model::Options)[:table] %}
        self.select((@alias || {{table.id.stringify}}) + ".*")
      {% end %}

      unless args.empty?
        self.select(*args)
      end

      self
    end

    # Add `SELECT` asterisk clause for the whole `T` table.
    #
    # ```
    # select(Post) # => SELECT posts.*
    # ```
    def select(klass : T.class)
      {% begin %}
        {% table = T.annotation(Model::Options)[:table] %}
        self.select((@alias || {{table.id.stringify}}) + ".*")
      {% end %}
    end

    @select : Deque(String)? = nil

    protected def get_select
      @select
    end

    protected def ensure_select
      @select = Deque(String).new if @select.nil?
      @select.not_nil!
    end

    protected def append_select(sql, *args)
      {% begin %}
        {% table = T.annotation(Model::Options)[:table] %}

        if selects = @select
          if selects.empty?
            sql << "SELECT " << (@alias || {{table.id.stringify}}) << ".*"
          else
            sql << "SELECT "

            first = true
            selects.each_with_index do |s, index|
              sql << ", " unless first; first = false
              sql << s
            end
          end
        else
          sql << "SELECT " << (@alias || {{table.id.stringify}}) << ".*"
        end

        sql << " FROM " << (@alias || {{table.id.stringify}})
      {% end %}
    end
  end
end
