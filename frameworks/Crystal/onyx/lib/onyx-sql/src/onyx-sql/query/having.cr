module Onyx::SQL
  class Query(T)
    # This method will raise in compilation-time,
    # because having a `HAVING` query with a `Model`'s attributes makes no sense.
    def having(or : Bool = false, not : Bool = false, **values : **U) : self forall U
      {% begin %}
        {% pretty_values = "" %}

        {% for key, value, index in U %}
          {% pretty_values = pretty_values + "#{key}: #{value}" %}
          {% pretty_values = pretty_values + ", " if index < (U.size - 1) %}
        {% end %}

        {% raise "Cannot call `Query(#{T})#having(#{pretty_values.id})` because `HAVING` clause with direct `#{T}` fields or references makes no sense. Use the string clause version instead" %}
      {% end %}
    end
  end
end
