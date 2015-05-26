require "moonshine"
require "redis"
require "html/builder"

include Moonshine
include Moonshine::Shortcuts
include Moonshine::Http

# Compose Objects (like Hash) to have a to_json method
require "json/to_json"

REDIS = Redis.new
app = Moonshine::App.new

class CONTENT
  UTF8 = "; charset=UTF-8"
  JSON = "application/json" + UTF8
  PLAIN = "text/plain"
  HTML = "text/html" + UTF8
end


app.response_middleware do |req, res|
    res.headers["Server"] = "Moonshine"
    res.headers["Date"] = Time.now.to_s
    res
end

private def randomWorld
  id = rand(1...10_000)
  {
    :id => id
    :randomNumber => REDIS.get("world:" + id.to_s)
  }
end

app.define do

  # Test 1: JSON Serialization
  get "/json", do |request|
    res = ok({ :message => "Hello, World!" }.to_json)
    res.headers["Content-type"] = CONTENT::JSON
    res
  end

  # Test 2: Single database query
  route "/db", do |request|
    res = ok(randomWorld.to_json)
    res.headers["Content-type"] = CONTENT::JSON
    res
  end

  # Test 3: Multiple database query
  route "/queries", do |request|
    queries = request.get["queries"].to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    results = (1..queries).map do
      randomWorld
    end

    res = ok(results.to_json)
    res.headers["Content-type"] = CONTENT::JSON
    res
  end

  # Test 4: Fortunes
  route "/fortunes", do |request|
    data = [] of  Hash(Symbol, (String | Int32))

    REDIS.lrange("fortunes", 0, -1).each_with_index do |e, i|
      data.push({ :id => i + 1, :message => e.to_s })
    end
    
    additional_fortune = {
      :id => 0
      :message => "Additional fortune added at request time."
    }
    data.push(additional_fortune)

    data.sort! { |a, b|
      a[:message].to_s <=> b[:message].to_s
    }

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

  # Test 6: Plaintext
  route "/plaintext", do |request|
    res = ok("Hello, World!")
    res.headers["Content-type"] = CONTENT::PLAIN
    res
  end

end

app.run(8080)
