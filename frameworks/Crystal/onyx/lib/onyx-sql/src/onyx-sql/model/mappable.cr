# This module allows to map a `Model` **to** the database.
module Onyx::SQL::Model::Mappable
  # Return a `Tuple` of DB-ready values. It respects `Field` and `Reference` annotations,
  # also working with `Converter`s.
  #
  # ```
  # User.db_values(id: user.id) # => {42}
  # User.db_values(foo: "bar")  # => Compilation-time error: unknown User instance foo
  # ```
  def self.db_values(**values : **U) : Tuple forall U
  end

  # Return a instance variable SQL column name by its *ivar_symbol*.
  #
  # NOTE: This method performs check in the runtime and if the matching variable is not
  # found by its name, it raises!
  #
  # ```
  # User.db_column(:id)      # "id"
  # User.db_column(:unknown) # Runtime error
  # ```
  #
  # TODO: Make it type-safe. See the [according forum topic](https://forum.crystal-lang.org/t/symbols/391).
  def self.db_column(ivar_symbol : Symbol) : String
  end

  macro included
    def self.db_values(**values : **U) : Tuple forall U
      {% verbatim do %}
        {% begin %}
          {% for ivar in @type.instance_vars %}
            {%
              a = 42

              unless ivar.type.nilable?
                raise "#{@type}@#{ivar.name} must be nilable, as it's an Onyx::SQL::Serializable variable"
              end

              unless ivar.type.union_types.size == 2
                raise "Only T | Nil unions can be an Onyx::SQL::Serializable's variables (got #{ivar.type} type for #{@type}@#{ivar.name})"
              end

              type = ivar.type.union_types.find { |t| t != Nil }

              if type <= Enumerable
                if (type.type_vars.size != 1 && type.type_vars.first.union?)
                  raise "If an Onyx::SQL::Serializable variable is a Enumerable, it must have a single non-union type var (got #{type} type for #{@type}@#{ivar.name})"
                end
              end
            %}
          {% end %}

          return {
            {% for key, value in U %}
              {% found = false %}

              {% for ivar in @type.instance_vars %}
                {% if ann = ivar.annotation(Onyx::SQL::Reference) %}
                  {% if key == ivar.name %}
                    {%
                      found = true

                      type = ivar.type.union_types.find { |t| t != Nil }
                      enumerable = false

                      if type <= Enumerable
                        enumerable = true
                        type = type.type_vars.first
                      end

                      options = type.annotation(Onyx::SQL::Model::Options)
                      raise "Onyx::SQL::Model::Options annotation must be defined for #{type}" unless options

                      pk = options[:primary_key]
                      raise "#{type} must have Onyx::SQL::Model::Options annotation with :primary_key option" unless pk

                      pk_rivar = type.instance_vars.find { |riv| "@#{riv.name}".id == pk.id }
                      raise "Cannot find primary key field #{pk} in #{type}" unless pk_rivar

                      pk_type = pk_rivar.type.union_types.find { |t| t != Nil }
                      converter = (a = pk_rivar.annotation(Onyx::SQL::Field)) && a[:converter]
                    %}

                    {% if enumerable %}
                      {% val = "values[#{key.symbolize}].try &.map(&.#{pk_rivar.name})".id %}
                    {% else %}
                      {% val = "values[#{key.symbolize}].try &.#{pk_rivar.name}".id %}
                    {% end %}

                    {% if converter %}
                      {{val}}.try { |v| {{converter}}.to_db(v).as(DB::Any) },
                    {% elsif pk_type <= DB::Any %}
                      {% if enumerable %}
                        {% raise "Cannot implicitly map enumerable reference #{@type}@#{ivar.name} to DB::Any. Consider applying a converter with `#to_db(Array(#{pk_type}))` method to #{type}@#{pk_rivar.name} to make it work" %}
                      {% else %}
                        {{val}}.as(DB::Any),
                      {% end %}
                    {% else %}
                      {% raise "Cannot implicitly map reference #{@type}@#{ivar.name} to DB::Any. Consider applying a converter with `#to_db(#{pk_type})` method to #{type}@#{pk_rivar.name} to make it work" %}
                    {% end %}
                  {% end %}
                {% else %}
                  {% if key == ivar.name %}
                    {%
                      found = true
                      type = ivar.type.union_types.find { |t| t != Nil }
                      converter = (a = ivar.annotation(Onyx::SQL::Field)) && a[:converter]
                    %}

                    {% if converter %}
                      (values[{{key.symbolize}}].try do |val|
                        {{converter}}.to_db(val).as(DB::Any)
                      end),
                    {% elsif type <= DB::Any %}
                      values[{{key.symbolize}}].as(DB::Any),
                    {% else %}
                      {% raise "Cannot implicitly map #{@type}@#{ivar.name} to DB::Any. Consider applying a converter with `#to_db(#{type})` method to #{@type}@#{ivar.name} to make it work" %}
                    {% end %}
                  {% end %}
                {% end %}
              {% end %}

              {% raise "Cannot find an instance variable named @#{key} in #{@type}" unless found %}
            {% end %}
          }
        {% end %}
      {% end %}
    end

    def self.db_column(ivar_symbol : Symbol) : String
      {% verbatim do %}
        {% begin %}
          case ivar_symbol
          {% for ivar in @type.instance_vars %}
            {% if ann = ivar.annotation(Onyx::SQL::Reference) %}
              {% if ann[:key] %}
                when {{ivar.name.symbolize}}, {{ann[:key].id.symbolize}}
                  return {{ann[:key].id.stringify}}
              {% else %}
                when {{ivar.name.symbolize}}
                  raise "Cannot map indirect {{@type}} reference @{{ivar.name}} to a DB column"
              {% end %}
            {% else %}
              when {{ivar.name.symbolize}}
                {% key = ((a = ivar.annotation(Onyx::SQL::Field)) && a[:key]) || ivar.name %}
                return {{key.id.stringify}}
            {% end %}
          {% end %}
          else
            raise "No instance variables found by symbol :#{ivar_symbol} in {{@type}}"
          end
        {% end %}
      {% end %}
    end
  end
end
