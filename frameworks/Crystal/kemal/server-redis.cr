require "kemal"
require "redis"
require "html_builder"

# Compose Objects (like Hash) to have a to_json method
require "json/to_json"

REDIS = Redis.new

class CONTENT
  UTF8 = "; charset=UTF-8"
  JSON = "application/json"
  PLAIN = "text/plain"
  HTML = "text/html" + UTF8
end

ID_MAXIMUM = 10_000

private def randomWorld
  id = rand(1..ID_MAXIMUM)
  num = REDIS.get("world:" + id.to_s)
  { :id => id, :randomNumber => num }
end

private def setWorld(world)
  id = "world:" + world[:id].to_s
  REDIS.set(id, world[:randomNumber])
  world
end

private def fortunes
  data = [] of  Hash(Symbol, (String | Int32))

  REDIS.lrange("fortunes", 0, -1).each_with_index do |e, i|
    data.push({:id => i + 1, :message => e.to_s})
  end
  data
end

private def sanitizedQueryCount(request)
  queries = request.params.query["queries"] as String
  return 1 if queries.empty? || queries.to_i?.nil?
  if queries.to_i > 500
    queries = 500
  elsif queries.to_i < 1
    queries = 1
  end
  queries.to_i
end

before_all do |env|
  env.response.headers["Server"] = "Kemal"
  env.response.headers["Date"] = Time.now.to_s
end

#
# Basic Tests
#

# Test 1: JSON Serialization
get "/json", do |env|
  env.response.content_type = CONTENT::JSON
  { :message => "Hello, World!" }.to_json
end

# Test 6: Plaintext
get "/plaintext", do |env|
  env.response.content_type = CONTENT::PLAIN
  "Hello, World!"
end

#
# Redis DatabaseTests
#

# Redis Test 2: Single database query
get "/db", do |env|
  env.response.content_type = CONTENT::JSON
  randomWorld.to_json
end

# Redis Test 3: Multiple database query
get "/queries", do |env|
  results = (1..sanitizedQueryCount(env)).map do
    randomWorld
  end

  env.response.content_type = CONTENT::JSON
  results.to_json
end

# Redis Test 4: Fortunes
get "/fortunes", do |env|
  data = fortunes

  additional_fortune = {
    :id => 0,
    :message => "Additional fortune added at request time."
  }
  data.push(additional_fortune)

  data.sort! do |a, b|
    a[:message].to_s <=> b[:message].to_s
  end

  # New builder for each request!
  html = HTML::Builder.new.build do
    html {
      head {
        title { text "Fortunes" }
      }
      body {
        table {
          tr {
            thead { text "id" }
            thead { text "message" }
          }
          data.each { |e|
            tr {
              td { text e[:id].to_s }
              td { text e[:message].to_s }
            }
          }
        }
      }
    }
  end

  # Doctype not available in builder
  # builder only supports `thead`, tests need to see `th`
  env.response.content_type = CONTENT::HTML
  "<!doctype html>" + html.gsub("thead", "th")
end

# Redis Test 5: Database Updates
get "/updates", do |env|
  updated = (1..sanitizedQueryCount(env)).map do
    world = randomWorld
    world[:randomNumber] = rand(1..ID_MAXIMUM)
    setWorld(world)
  end

  env.response.content_type = CONTENT::JSON
  updated.to_json
end
