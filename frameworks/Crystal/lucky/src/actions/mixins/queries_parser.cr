module QueriesParser
  def queries_param : Int32
    queries = params.get?("queries") || "1"
    queries = queries.to_i? || 1
    queries.clamp(1..500)
  end
end
