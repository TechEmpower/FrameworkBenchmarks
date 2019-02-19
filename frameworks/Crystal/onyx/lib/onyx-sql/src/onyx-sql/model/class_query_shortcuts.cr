require "../query"

# This module is **extended** by an object whenever it includes the `Model` module.
# It brings shortucts to a matching query initialization:
#
# ```
# class User
#   include Onyx::SQL::Model
# end
#
# User.query == Query(User).new
# ```
module Onyx::SQL::Model::ClassQueryShortcuts
  # Create a new `Query(self)`.
  def query : Query
    Query(self).new
  end

  {% for method in %w(
                     delete
                     group_by
                     insert
                     limit
                     offset
                     returning
                     select
                     set
                     update
                     where
                     having
                     all
                     one
                     first
                     last
                   ) %}
    # Create a new `Query(self)` and call `Query#{{method.id}}` on it.
    def {{method.id}}(*args, **nargs) : Query
      query.{{method.id}}(*args, **nargs)
    end
  {% end %}

  # Create a new `Query(self)` and call `Query#order_by` on it.
  def order_by(column, order : Query::Order? = nil) : Query
    query.order_by(column, order)
  end

  # Create a new `Query(self)` and call `Query#join` on it.
  def join(table : String, on : String, as _as : String? = nil, type : Onyx::SQL::Query::JoinType = :inner) : Query
    query.join(table, on, _as, type)
  end

  # Create a new `Query(self)` and call `Query#join` on it.
  def join(reference : Symbol, on : String? = nil, as _as : String? = nil, type : Onyx::SQL::Query::JoinType = :inner) : Query
    query.join(reference, on, _as, type)
  end
end
