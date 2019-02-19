require "./changes"
require "../query"

module Onyx::SQL::Model
  # A shortcut method to genereate an insert `Query` pre-filled with actual `self` values.
  # See `Query#insert`.
  #
  # ```
  # user = User.new(id: 42, name: "John")
  # user.insert == Query(User).new.insert(id: 42, name: "John")
  # ```
  def insert : Query
    {% begin %}
      Query(self).new.insert(
        {% for ivar in @type.instance_vars %}
          # Skip foreign references. Because they are foreign, you know
          {% unless ((a = ivar.annotation(Reference)) && !a[:key]) %}
            {{ivar.name}}: @{{ivar.name}},
          {% end %}
        {% end %}
      )
    {% end %}
  end

  # A shortcut method to genereate an update `Query` with *changeset* values.
  # See `Query#update` and `Query#set`.
  #
  # ```
  # user = User.new(id: 42, name: "John")
  # changeset = user.changeset
  # changeset.update(name: "Jake")
  # user.update(changeset) == Query(User).new.update.set(name: "Jake").where(id: 42)
  # ```
  def update(changeset : Changeset(self, U)) : Query forall U
    query = Query(self).new.update

    {% begin %}
      changeset.changes!.each do |key, value|
        case key
        {% for ivar in @type.instance_vars %}
          when {{ivar.name.stringify}}
            query.set({{ivar.name}}: value.as({{ivar.type}}))
        {% end %}
        else
          raise "BUG: Unrecognized Changeset({{@type}}) key :#{key}"
        end
      end
    {% end %}

    where_self(query)
  end

  # A shortcut method to genereate a delete `Query`.
  # See `Query#delete`.
  #
  # ```
  # user = User.new(id: 42)
  # user.delete == Query(User).new.delete.where(id: 42)
  # ```
  def delete : Query
    query = Query(self).new.delete
    where_self(query)
  end

  protected def where_self(query : Query)
    {% begin %}
      {%
        options = @type.annotation(Model::Options)
        raise "Onyx::SQL::Model::Options annotation must be defined for #{@type}" unless options

        pk = options[:primary_key]
        raise "Onyx::SQL::Model::Options annotation is missing :primary_key option for #{@type}" unless pk

        pk_ivar = @type.instance_vars.find { |iv| "@#{iv.name}".id == pk.id }
        raise "Cannot find primary key field #{pk} for #{@type}" unless pk_ivar
      %}

      query.where({{pk_ivar.name}}: @{{pk_ivar.name}})
    {% end %}
  end
end
