module Onyx::SQL
  class Query(T)
    # Mark this query as `UPDATE` one. This is for convenience only, and the `#build` would
    # raise if there are no `SET` clauses in the query.
    #
    # ```
    # User.update.build             # Runtime error: No values to SET in the UPDATE query
    # User.update.set(name: "Jane") # OK
    # ```
    #
    # `Model`s have a handy `Model#update` shortcut:
    #
    # ```
    # changeset = user.changeset
    # changeset.update(name: "Jake")
    # user.update(changeset) == User.update.set(name: "Jake").where(id: user.id)
    # ```
    def update
      @type = :update
      self
    end

    protected def append_update(sql, *args)
      {% begin %}
        {% table = T.annotation(SQL::Model::Options)[:table] %}
        sql << "UPDATE " << (@alias || {{table.id.stringify}})
      {% end %}
    end
  end
end
