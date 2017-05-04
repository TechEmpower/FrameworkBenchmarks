require "kemal"
require "redis"

# Compose Objects (like Hash) to have a to_json method
require "json/to_json"

REDIS = Redis.new

class CONTENT
  UTF8  = "; charset=UTF-8"
  JSON  = "application/json"
  PLAIN = "text/plain"
  HTML  = "text/html" + UTF8
end

ID_MAXIMUM = 10_000

private def random_world
  id = rand(1..ID_MAXIMUM)
  num = REDIS.get("world:#{id}")
  {id: id, randomNumber: num}
end

private def set_world(world)
  id = "world:#{world[:id]}"
  REDIS.set(id, world[:randomNumber])
  world
end

private def fortunes
  data = [] of NamedTuple(id: Int32, message: String)

  REDIS.lrange("fortunes", 0, -1).each_with_index do |e, i|
    data.push({id: i + 1, message: e.to_s})
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
# Redis DatabaseTests
#

# Redis Test 2: Single database query
get "/db" do |env|
  env.response.content_type = CONTENT::JSON
  random_world.to_json
end

# Redis Test 3: Multiple database query
get "/queries" do |env|
  results = (1..sanitized_query_count(env)).map do
    random_world
  end

  env.response.content_type = CONTENT::JSON
  results.to_json
end

# Redis Test 4: Fortunes
get "/fortunes" do |env|
  data = fortunes

  additional_fortune = {
    id:      0,
    message: "Additional fortune added at request time.",
  }
  data.push(additional_fortune)

  data.sort_by! { |fortune| fortune[:message] }

  render "views/fortunes.ecr"
end

# Redis Test 5: Database Updates
get "/updates" do |env|
  updated = (1..sanitized_query_count(env)).map do
    set_world({id: random_world[:id], randomNumber: rand(1..ID_MAXIMUM)})
  end

  env.response.content_type = CONTENT::JSON
  updated.to_json
end

logging false
Kemal.run { |cfg| cfg.server.bind(reuse_port: true) }
