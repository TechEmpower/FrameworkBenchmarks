require "raze"
require "pg"

BENCH_DB = DB.open(ENV["DATABASE_URL"])

class CONTENT
  ID_MAX = 10_000
  JSON  = "application/json"
  PLAIN = "text/plain"
  HTML  = "text/html; charset=UTF-8"
end

private def get_world
  id = Random.rand(CONTENT::ID_MAX).succ
  random_number = BENCH_DB.query_one("SELECT id, randomNumber FROM world WHERE id = $1", id, as: Int32)
  { id: id, randomNumber: random_number }
end

private def set_world(world)
  BENCH_DB.exec("UPDATE world SET randomNumber = $1 WHERE id = $2", world[:randomNumber], world[:id])
  world
end

private def fortunes
  data = Array(NamedTuple(id: Int32, message: String)).new

  BENCH_DB.query_each("SELECT id, message FROM Fortune") do |rs|
    data.push({id: rs.read(Int32), message: rs.read(String)})
  end

  data
end

private def sanitized_query_count(ctx)
  queries = ctx.query["queries"].as(String)
  queries = queries.to_i? || 1
  queries.clamp(1..500)
end

# Test 1: JSON Serialization
get "/json" do |ctx|
  ctx.response.headers["Server"] = "Raze"
  ctx.response.headers["Date"] = HTTP.format_time(Time.now)
  ctx.response.content_type = CONTENT::JSON
  { message: "Hello, World!" }.to_json
end

# Postgres Test 2: Single database query
get "/db" do |ctx|
  ctx.response.headers["Server"] = "Raze"
  ctx.response.headers["Date"] = HTTP.format_time(Time.now)
  ctx.response.content_type = CONTENT::JSON
  get_world.to_json
end

# Postgres Test 3: Multiple database query
get "/queries" do |ctx|
  results = (1..sanitized_query_count(ctx)).map do
    get_world
  end
  ctx.response.headers["Server"] = "Raze"
  ctx.response.headers["Date"] = HTTP.format_time(Time.now)
  ctx.response.content_type = CONTENT::JSON
  results.to_json
end

# Postgres Test 4: Fortunes
get "/fortunes" do |ctx|
  ctx.response.headers["Server"] = "Raze"
  ctx.response.headers["Date"] = HTTP.format_time(Time.now)
  ctx.response.content_type = CONTENT::HTML
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
get "/updates" do |ctx|
  updated = (1..sanitized_query_count(ctx)).map do
    set_world({id: get_world[:id], randomNumber: Random.rand(CONTENT::ID_MAX).succ})
  end
  ctx.response.headers["Server"] = "Raze"
  ctx.response.headers["Date"] = HTTP.format_time(Time.now)
  ctx.response.content_type = CONTENT::JSON
  updated.to_json
end

# Test 6: Plaintext
get "/plaintext" do |ctx|
  ctx.response.headers["Server"] = "Raze"
  ctx.response.headers["Date"] = HTTP.format_time(Time.now)
  ctx.response.content_type = CONTENT::PLAIN
  "Hello, World!"
end

Raze.config.logging = false
Raze.config.port = 8080
Raze.config.env = "production"
Raze.config.reuse_port = true
Raze.run
