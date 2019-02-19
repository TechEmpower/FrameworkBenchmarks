module Onyx::SQL
  class Query(T)
    # The SQL join type.
    enum JoinType
      Inner
      Left
      Right
      Full
    end

    # Add explicit `JOIN` clause.
    #
    # ```
    # query.join("a", on: "a.id = b.id", as: "c", :right) # => RIGHT JOIN a ON a.id = b.id AS c
    # ```
    def join(table : String, on : String, as _as : String? = nil, type : JoinType = :inner)
      ensure_join << Join.new(
        table: table,
        on: on,
        as: _as,
        type: type
      )

      self
    end

    # Add `JOIN` clause by **type-safe** reference.
    # Yields another `Query` instance which has the reference's type.
    # It then merges the yielded query with the main query.
    #
    # ```
    # class User
    #   include Onyx::SQL::Model
    #
    #   schema users do
    #     pkey id : Int32
    #     type username : String
    #     type authored_posts : Array(Post), foreign_key: "author_id"
    #   end
    # end
    #
    # class Post
    #   include Onyx::SQL::Model
    #
    #   schema posts do
    #     pkey id : Int32
    #     type body : String
    #     type author : User, key: "author_id"
    #   end
    # end
    #
    # query = Post
    #   .select(Post)
    #   .join(author: true) do |q|
    #     pp typeof(q)        # => Query(User)
    #     q.select(:username) # :username is looked up in User in compilation time
    #     q.where(id: 42)
    #   end
    #
    # query.build # => {"SELECT posts.*, author.username FROM posts INNER JOIN users ON posts.author_id = author.id AS author WHERE author.id = ?", {42}}
    # ```
    #
    # In fact, the resulting SQL slightly differs from the example above:
    #
    # ```text
    # SELECT posts.*, '' AS _author, author.username, '' AS _author FROM posts ...
    # ```
    #
    # The `"AS _author"` thing is a *marker*, which is used to preload references on
    # `Serializable.from_rs` call:
    #
    # ```
    # posts = repo.query(query)
    # pp posts # => [<Post @author=<User @id=42 @username="John">>, <Post @author=<User @id=42 @username="John">>, ...]
    # ```
    #
    # Read more about preloading references in `Serializable` docs.
    #
    # NOTE: The syntax is about to be improved from `join(author: true)` to `join(:author)`.
    # See the relevant [forum topic](https://forum.crystal-lang.org/t/symbols/391).
    def join(*, on : String? = nil, as _as : String? = nil, type : JoinType = :inner, **values : **U, &block) : self forall U
      {% begin %}
        {%
          raise "Can only join a single reference" if U.keys.size != 1
          key = U.keys.first

          ivar = T.instance_vars.find { |iv| iv.name == key }
          raise "Cannot find instance variable @#{key} in #{T}" unless ivar

          ann = ivar.annotation(Reference)
          raise "Instance variable @#{key} of #{T} must have Onyx::SQL::Reference annotation" unless ann
        %}

        join({{ivar.name.symbolize}}, on: on, as: _as, type: type)

        {%
          type = ivar.type.union_types.find { |t| t != Nil }
          append_select = true

          if type <= Enumerable
            # Do not append select on foreign enumerable references,
            # because that would result in multiple rows
            append_select = false unless ann[:key]

            if (type.type_vars.size != 1 && type.type_vars.first.union?)
              raise "Cannot use #{type} as #{T}@#{ivar.name} reference"
            end

            type = type.type_vars.first
          end
        %}

        subquery =  Query({{type}}).new(_as || {{ivar.name.stringify}})
        yield subquery

        {% if append_select %}
          if sub_select = subquery.get_select
            self.select("'' AS _{{ivar.name}}")
            ensure_select.concat(sub_select)
            self.select("'' AS _{{ivar.name}}")
          end

          ensure_order_by.concat(subquery.get_order_by.not_nil!) if subquery.get_order_by
        {% end %}

        ensure_join.concat(subquery.get_join.not_nil!) if subquery.get_join
        ensure_where.concat(subquery.get_where.not_nil!) if subquery.get_where
      {% end %}

      self
    end

    # Add `JOIN` clause by **type-UNsafe** *reference*. This method would **not** check
    # in compilation time and the reference will looked up in the `T` in runtime.
    # It does not yield the reference subquery, unlike the type-safe `#join` variant.
    # It's because of language limitations and may change in the future. See [this forum topic](https://forum.crystal-lang.org/t/symbols/391) for details.
    #
    # ```
    # query = Post.join(:author).where(id: 17)
    # query.build # => {"SELECT posts.* FROM posts INNER JOIN users AS author ON posts.author_id = author.id WHERE posts.id = ?", {17}}
    # ```
    #
    # To select from the joined table, unlike the type-safe `#join` variant, you have to call it explicitly:
    #
    # ```
    # query = Post.select(Post, "author.username").join(:author)
    # # SELECT posts.*, author.username FROM posts
    # ```
    #
    # Note that there are no markers, so a post's `@author` reference would not
    # have `@username` variable filled. However, the refernece itself would still present,
    # as a post row itself contains the `"author_id"` column, which would be put into
    # a post's `@author` instance upon calling `Serializable.from_rs`:
    #
    # ```
    # post = repo.query(Post.select(Post, "author.username").join(:author)).first
    # pp post # => <Post @author=<User @id=... @username=nil>>
    # ```
    #
    # TODO: Make it type-safe.
    def join(reference : Symbol, on : String? = nil, as _as : String? = nil, type : JoinType = :inner)
      {% begin %}
        {%
          table = T.annotation(Model::Options)[:table].id
          pkey = T.instance_vars.find do |ivar|
            "@#{ivar.name}".id == T.annotation(Model::Options)[:primary_key].id
          end
        %}

        case reference
        {% for ivar in T.instance_vars %}
          {% if ann = ivar.annotation(Reference) %}
            {%
              type = ivar.type.union_types.find { |t| t != Nil }
              enumerable = false

              if type <= Enumerable
                enumerable = true
                type = type.type_vars.first
              end

              roptions = type.annotation(Model::Options)
              raise "Onyx::SQL::Model::Options annotation must be defined for #{type}" unless roptions

              rtable = roptions[:table].id
              raise "Onyx::SQL::Model::Options annotation is missing :table option for #{type}" unless rtable
            %}

            when {{ivar.name.symbolize}}
              {% if key = ann[:key] %}
                {%
                  rpk = roptions[:primary_key]
                  raise "Onyx::SQL::Model::Options annotation is missing :primary_key option for #{type}" unless rpk

                  rpk = rpk.name.stringify.split('@')[1].id
                  on_op = (enumerable ? "IN".id : "=".id)
                %}

                ensure_join << Join.new(
                  table: {{rtable.stringify}},
                  on: on || "#{_as || {{ivar.name.stringify}}}.#{ {{type}}.db_column({{rpk.symbolize}}) } {{on_op}} #{@alias || {{table.stringify}}}.{{key.id}}",
                  as: _as || {{ivar.name.stringify}},
                  type: type
                )
              {% elsif foreign_key = ann[:foreign_key] %}
                {%
                  rivar = type.instance_vars.find do |rivar|
                    (a = rivar.annotation(Reference)) && (a[:key].id == foreign_key.id)
                  end
                  raise "Cannot find matching reference for #{T}@#{ivar.name} in #{type}" unless rivar

                  rtype = rivar.type.union_types.find { |t| t != Nil }
                  key = rivar.annotation(Reference)[:key].id
                  on_op = rtype <= Enumerable ? "IN".id : "=".id
                %}

                ensure_join << Join.new(
                  table: {{rtable.stringify}},
                  on: on || "#{@alias || {{table.stringify}}}.#{T.db_column({{pkey.name.symbolize}})} {{on_op}} #{_as || {{ivar.name.stringify}}}.{{key}}",
                  as: _as || {{ivar.name.stringify}},
                  type: type
                )
              {% else %}
                {% raise "Neither key nor foreign_key is set for #{ivar.name} in #{T}" %}
              {% end %}
          {% end %}
        {% end %}
        else
          raise "Cannot find reference with key :#{reference} for #{T}"
        end
      {% end %}

      self
    end

    # TODO: Make this work
    # {% for x in %w(inner left right full) %}
    #   def {{x.id}}_join(**values, &block)
    #     join(**values, type: {{x.id.symbolize}}, &block)
    #   end
    # {% end %}

    private struct Join
      getter table, on, _as, type

      def initialize(@table : String, @on : String, as @_as : String?, @type : JoinType)
      end
    end

    @join : Deque(Join)? = nil

    protected def get_join
      @join
    end

    protected def ensure_join
      @join ||= Deque(Join).new
    end

    protected def append_join(sql, *args)
      return unless @join

      ensure_join.each do |join|
        clause = case join.type
                 when .inner? then " INNER JOIN "
                 when .left?  then " LEFT JOIN "
                 when .right? then " RIGHT JOIN "
                 when .full?  then " FULL JOIN "
                 end

        sql << clause << join.table

        if (_as = join._as) && !_as.empty?
          sql << " AS " << join._as
        end

        unless join.on.empty?
          sql << " ON " << join.on
        end
      end
    end
  end
end
