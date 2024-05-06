# Helper methods defined here can be accessed in any controller or view in the application

MAX_PK = 10_000
QUERIES_MIN = 1
QUERIES_MAX = 500

HelloWorld::App.helpers do
  def rand1
    rand(MAX_PK) + 1
  end

  def bounded_queries
    queries = params[:queries].to_i
    queries.clamp(QUERIES_MIN, QUERIES_MAX)
  end
end
