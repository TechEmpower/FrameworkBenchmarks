module Onyx::SQL
  class Query(T)
    # Mark this query as a `DELETE` one. It's recommended to call `#where` as well:
    #
    # ```
    # query = User.delete.where(id: 17)
    # query.build # => {"DELETE FROM users WHERE id = ?", {17}}
    # ```
    #
    # `Model`s have a handy `Model#delete` shortcut:
    #
    # ```
    # user.delete == User.delete.where(id: user.id)
    # ```
    def delete
      @type = :delete
      self
    end

    protected def append_delete(sql, *args)
      {% begin %}
        {% table = T.annotation(SQL::Model::Options)[:table] %}
        sql << "DELETE FROM " << (@alias || {{table.id.stringify}})
      {% end %}
    end
  end
end
