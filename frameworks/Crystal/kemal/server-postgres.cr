require "kemal"
require "pg"
require "pool/connection"

# Compose Objects (like Hash) to have a to_json method
require "json/to_json"

DB = ConnectionPool.new(capacity: 25, timeout: 0.01) do
  PG.connect("postgres://benchmarkdbuser:benchmarkdbpass@#{ENV["DBHOST"]? || "127.0.0.1"}/hello_world")
end

class CONTENT
  UTF8  = "; charset=UTF-8"
  JSON  = "application/json"
  PLAIN = "text/plain"
  HTML  = "text/html" + UTF8
end

ID_MAXIMUM = 10_000

private def random_world
  id = rand(1..ID_MAXIMUM)
  conn = DB.checkout
  result = conn.exec({Int32, Int32}, "SELECT id, randomNumber FROM world WHERE id = $1", [id]).rows.first
  DB.checkin(conn)
  {id: result[0], randomNumber: result[1]}
end

private def set_world(world)
  conn = DB.checkout
  result = conn.exec("UPDATE world set randomNumber = $1 where id = $2", [world[:randomNumber], world[:id]])
  DB.checkin(conn)
  world
end

private def fortunes
  data = [] of NamedTuple(id: Int32, message: String)

  DB.connection.exec({Int32, String}, "select id, message from Fortune").rows.each do |row|
    data.push({id: row[0], message: row[1]})
  end
  data
end

private def sanitized_query_count(request)
  queries = request.params.query["queries"].as(String)
  queries = queries.to_i? || 1
  queries.clamp(1..500)
end

before_all do |env|
  env.response.headers["Server"] = "Kemal"
  env.response.headers["Date"] = Time.now.to_s
end

#
# Basic Tests
#

# Test 1: JSON Serialization
get "/json" do |env|
  env.response.content_type = CONTENT::JSON
  {message: "Hello, World!"}.to_json
end

# Test 6: Plaintext
get "/plaintext" do |env|
  env.response.content_type = CONTENT::PLAIN
  "Hello, World!"
end

#
# Postgres DatabaseTests
#

# Postgres Test 2: Single database query
get "/db" do |env|
  env.response.content_type = CONTENT::JSON
  random_world.to_json
end

# Postgres Test 3: Multiple database query
get "/queries" do |env|
  results = (1..sanitized_query_count(env)).map do
    random_world
  end

  env.response.content_type = CONTENT::JSON
  results.to_json
end

# Postgres Test 4: Fortunes
get "/fortunes" do |env|
  env.response.content_type = CONTENT::HTML
  data = fortunes
  additional_fortune = {
    id:      0,
    message: "Additional fortune added at request time.",
  }
  data.push(additional_fortune)

  data.sort_by! { |fortune| fortune[:message] }

  render "views/fortunes.ecr"
end

# Postgres Test 5: Database Updates
get "/updates" do |env|
  updated = (1..sanitized_query_count(env)).map do
    set_world({id: random_world[:id], randomNumber: rand(1..ID_MAXIMUM)})
  end

  env.response.content_type = CONTENT::JSON
  updated.to_json
end

logging false
Kemal.run
