require "test/unit"
require "rack"
require "rack/test"
require "oj"
require_relative "hello_world.rb"
require "prettyprint"
#OUTER_APP = Rack::Builder.parse_file("config.ru")

class TestApp < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    HelloWorld.new
  end

  def test_json
    get "/json"
    pp last_response
    assert last_response.ok?
    assert last_response.headers["content-type"] == "application/json"
  end
  def test_plaintext
    get "/plaintext"
    assert last_response.ok?
    assert last_response.headers["content-type"] == "text/plain"
  end
end
