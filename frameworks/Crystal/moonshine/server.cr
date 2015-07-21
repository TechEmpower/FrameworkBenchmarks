require "moonshine"
require "redis"
require "html/builder"

include Moonshine
include Moonshine::Utils::Shortcuts
include Moonshine::Base

# Compose Objects (like Hash) to have a to_json method
require "json/to_json"

REDIS = Redis.new
app = App.new

class CONTENT
  UTF8 = "; charset=UTF-8"
  JSON = "application/json" + UTF8
  PLAIN = "text/plain"
  HTML = "text/html" + UTF8
end

ID_MAXIMUM = 10_000

app.response_middleware do |req, res|
    res.headers["Server"] = "Moonshine"
    res.headers["Date"] = Time.now.to_s
    res
end

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

private def sanitizedQueryCount(request)
  begin
    queries = request.get["queries"].to_i
  rescue
    queries = 1
  end
  queries = 1 if queries < 1
  queries = 500 if queries > 500
  queries
end

#
# Basic Tests
#

# Test 1: JSON Serialization
app.get "/json", do |request|
  res = ok({ :message => "Hello, World!" }.to_json)
  res.headers["Content-type"] = CONTENT::JSON
  res
end

# Test 6: Plaintext
app.get "/plaintext", do |request|
  res = ok("Hello, World!")
  res.headers["Content-type"] = CONTENT::PLAIN
  res
end

#
# Redis DatabaseTests
#

# Redis Test 2: Single database query
app.get "/redis/db", do |request|
  res = ok(randomWorld.to_json)
  res.headers["Content-type"] = CONTENT::JSON
  res
end

# Redis Test 3: Multiple database query
app.get "/redis/queries", do |request|
  results = (1..sanitizedQueryCount(request)).map do
    randomWorld
  end

  res = ok(results.to_json)
  res.headers["Content-type"] = CONTENT::JSON
  res
end

# Redis Test 4: Fortunes
app.get "/redis/fortunes", do |request|
  data = [] of  Hash(Symbol, (String | Int32))

  REDIS.lrange("fortunes", 0, -1).each_with_index do |e, i|
    data.push({ :id => i + 1, :message => e.to_s })
  end
  
  additional_fortune = {
    :id => 0
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
  res = ok("<!doctype html>" + html.gsub("thead", "th"))
  res.headers["Content-type"] = CONTENT::HTML
  res
end

# Redis Test 5: Database Updates
app.get "/redis/updates", do |request|
  updated = (1..sanitizedQueryCount(request)).map do
    world = randomWorld
    world[:randomNumber] = rand(1..ID_MAXIMUM)
    setWorld(world)
  end

  res = ok(updated.to_json)
  res.headers["Content-type"] = CONTENT::JSON
  res
end

app.run(8080)
