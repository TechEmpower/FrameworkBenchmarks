require "../../spec_helper"
require "../../../src/onyx-rest/renderers/text"

struct TextView
  include Onyx::REST::View

  def initialize(@foo : String)
  end

  text("foo: #{@foo}")

  # `Renderers::JSON` is required too in another spec,
  # therefore Crystal assumes this view could be invoked with #to_json as well
  json(raise NotImplementedError.new(self))
end

class TextError < Onyx::REST::Error(505)
  def initialize(@foo : String)
    super(@foo)
  end

  def payload
    {foo: @foo}
  end
end

class TextRendererSpecServer
  def initialize
    renderer = Onyx::REST::Renderers::Text.new
    router = Onyx::HTTP::Router.new do
      get "/" do |env|
        if env.request.query_params["raise"]?
          env.response.error = TextError.new("Boom!")
        else
          env.response.view = TextView.new("OK")
        end
      end
    end

    @server = Onyx::HTTP::Server.new([router, renderer])
  end

  getter server
end

describe Onyx::REST::Renderers::Text do
  server = TextRendererSpecServer.new

  spawn do
    server.server.bind_tcp(4890)
    server.server.listen
  end

  sleep(0.1)

  client = HTTP::Client.new(URI.parse("http://localhost:4890"))

  describe "view rendering" do
    it do
      response = client.get("/")
      response.status_code.should eq 200
      response.headers["Content-Type"].should eq "text/plain; charset=utf-8"
      response.body.should eq "foo: OK"
    end
  end

  describe "error rendering" do
    it do
      response = client.get("/?raise=true")
      response.status_code.should eq 505
      response.headers["Content-Type"].should eq "text/plain; charset=utf-8"
      response.body.should eq "505 Boom!"
    end
  end

  server.server.close
end
