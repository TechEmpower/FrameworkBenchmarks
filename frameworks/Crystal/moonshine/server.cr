require "moonshine"
require "redis"

include Moonshine
include Moonshine::Shortcuts
include Moonshine::Http

# Compose Objects (like Hash) to have a to_json method
require "json/to_json"

redis = Redis.new
app = Moonshine::App.new

class CONTENT
  JSON = "application/json; charset=UTF-8"
  PLAIN = "text/plain"
end


app.response_middleware do |req, res|
    res.headers["Server"] = "Moonshine"
    res.headers["Date"] = Time.now.to_s
    res
end

app.define do

  # Test 1: JSON Serialization
  get "/json", do |request|
    res = ok({ :message => "Hello, World!" }.to_json)
    res.headers["Content-type"] = CONTENT::JSON
    res
  end

  # Test 4: Fortunes
  route "/fortunes", do |request|
    res = ok({
      :length => redis.llen("fortunes")
    }.to_json)
    res.headers["Content-type"] = CONTENT::JSON
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
