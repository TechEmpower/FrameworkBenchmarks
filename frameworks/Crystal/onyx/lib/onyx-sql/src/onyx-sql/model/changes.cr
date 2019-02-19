module Onyx::SQL::Model
  # A changeset for a model. Used to track changes made to it.
  # To update a value in a changeset, call `#update`. This will **not** affect the original model.
  #
  # It is instantiated via `Model#changeset`:
  #
  # ```
  # user = User.new(name: "John")
  # changeset = user.changeset
  # changeset.update(name: "Jake")
  # pp changeset.changes # => {"name" => "Jake"}
  # ```
  #
  # It is handy to use a changeset with `Model::InstanceQueryShortcuts#update` method.
  # Note that `Model::InstanceQueryShortcuts#update` may raise `NoChanges` error
  # if the changeset is empty.
  class Changeset(T, U)
    getter values : Hash(String, U)
    getter initial_values : Hash(String, U)

    protected def initialize(@initial_values : Hash(String, U))
      @values = @initial_values.dup
    end

    # Update a changeset value by `T` instance varialbe.
    # This will **not** affect the original model.
    def update(**values : **V) : Nil forall V
      values.each do |key, value|
        {% begin %}
          case key
          {% for key, value in V %}
            {% found = false %}

            {% for ivar in T.instance_vars %}
              {% if ivar.name == key %}
                {% found = true %}

                when {{ivar.name.symbolize}}
                  @values[{{ivar.name.stringify}}] = value.as({{value}})
              {% end %}
            {% end %}

            {% raise "Cannot find instance variable by key :#{key} in #{T}" unless found %}
          {% end %}
          else
            raise "BUG: Unmatched :#{key} in runtime"
          end
        {% end %}
      end
    end

    # Return changes on this changeset.
    def changes : Hash(String, U)
      hash = Hash(String, U).new

      values.each do |key, value|
        if value.responds_to?(:primary_key)
          if !initial_values.has_key?(key) || (value.primary_key && initial_values[key] != value)
            hash[key] = value
          end
        else
          if !initial_values.has_key?(key) || initial_values[key] != value
            hash[key] = value
          end
        end
      end

      hash
    end

    # Return `true` if there are no changes on this changeset.
    def empty?
      changes.empty?
    end

    # Return `#changes` on this changeset or raise `NoChanges` if it's empty.
    def changes!
      actual_changes = changes
      raise NoChanges.new if actual_changes.empty?
      actual_changes
    end

    # Raised when there are no actual changes on a changeset on `#changes!` call.
    class NoChanges < Exception
    end
  end

  # Create a new changeset for this instance with snapshot of actual values.
  # It is then likely to be passed to the `#update` method.
  #
  # ```
  # user = User.new(id: 42, name: "John")
  # changeset = user.changeset
  # pp changeset.initial_values # => {"id" => 42, "name" => "John"}
  # pp changeset.values         # => {"id" => 42, "name" => "John"}
  #
  # changeset.update(name: "Jake")
  # pp changeset.values  # => {"id" => 42, "name" => "Jake"}
  # pp changeset.empty?  # => false
  # pp changeset.changes # => {"name" => "Jake"}
  #
  # user.update(changeset) == User.update.set(name: "Jake").where(id: 42)
  # ```
  def changeset
    {% begin %}
      hash = Hash(String, Union(
        {% for ivar in @type.instance_vars %}
          {{ivar.type}},
        {% end %}
      )).new

      {% for ivar in @type.instance_vars %}
        unless @{{ivar.name}}.nil?
          hash[{{ivar.name.stringify}}] = @{{ivar.name}}
        end
      {% end %}

      Changeset(self, Union(
        {% for ivar in @type.instance_vars %}
          {{ivar.type}},
        {% end %}
      )).new(hash)
    {% end %}
  end

  # Apply a *changeset*, merging self values with the changeset's.
  def apply(changeset : Changeset(self, U)) : self forall U
    changeset.changes.each do |key, value|
      {% begin %}
        case key
        {% for ivar in @type.instance_vars %}
          when {{ivar.stringify}}
            @{{ivar}} = value.as({{ivar.type}})
        {% end %}
        else
          raise "BUG: Unknown key :#{key} for Changeset(#{self})"
        end
      {% end %}
    end

    self
  end
end
