require "pg"
require "onyx/sql"
require "onyx/http"

require "./models/*"
require "./views/*"
require "./endpoints/**"

class CustomHandler
  include HTTP::Handler

  def call(context)
    context.response.headers["Server"] = "Onyx"
    context.response.headers["Date"] = HTTP.format_time(Time.now)
    call_next(context)
  end
end

Onyx.draw do
  get "/json" do |env|
    env.response.content_type = "application/json"
    {message: "Hello, World!"}.to_json(env.response)
  end

  get "/db", Endpoints::Worlds::Random
  get "/queries", Endpoints::Worlds::Many
  get "/updates", Endpoints::Worlds::Update

  get "/plaintext" do |env|
    env.response.content_type = "text/plain"
    env.response << "Hello, World!"
  end
end

Onyx.listen(ENV["TEST_HOST"], 8080) do
  handlers.unshift(CustomHandler.new)
end
