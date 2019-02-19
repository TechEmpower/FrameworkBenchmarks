struct Tuple
  # Similar to `Array#[](range)`, but for a tuple.
  def [](range : Range(Int, Int))
    result = Tuple.new

    {% for t, i in T %}
      if ({{i}} >= range.begin) && (
        range.end > 0 ? {{i}} <= range.end : {{i}} <= {{T.size}} + range.end
      )
        result = result + { at({{i}}) }
      end
    {% end %}

    result
  end
end
