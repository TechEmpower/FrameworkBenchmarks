require "moonshine"
include Moonshine
include Moonshine::Shortcuts
include Moonshine::Http

# Compose Objects (like Hash) to have a to_json method
require "json/to_json"

app = Moonshine::App.new

is = {
  :JSON => "application/json; charset=UTF-8"
  :PLAIN => "text/plain"
}

app.response_middleware do |req, res|
    res.headers["Server"] = "Moonshine"
    res.headers["Date"] = Time.now.to_s
    res
end

app.define do

  route "/plaintext", do |request|
    res = ok("Hello, World!")
    res.headers["Content-type"] = is[:PLAIN]
    res
  end

  get "/json", do |request|
    res = ok({ :message => "Hello, World!" }.to_json)
    res.headers["Content-type"] = is[:JSON]
    res
  end

end

app.run(8080)
