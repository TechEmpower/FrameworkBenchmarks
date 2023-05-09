#!/usr/bin/env ruby
require "test/unit"
require "rack"
require "rack/test"
require "oj"
require_relative "hello_world.rb"
require "prettyprint"
ROUTER_APP = Rack::Builder.parse_file("config.ru")

class TestApp < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    #HelloWorld.new
    ROUTER_APP
  end
  # def test_db
  #  app.test_database
  # end
  def test_json
    get "/json"

    assert last_response.ok?
    assert last_response.headers["content-type"] == "application/json"
  end
  def test_plaintext
    get "/plaintext"
    assert last_response.ok?
    assert last_response.headers["content-type"] == "text/plain"
  end
  def test_fortunes
    get "/fortunes"
    assert last_response.ok?
    assert last_response.headers["content-type"] == "text/html; charset=utf-8"
  end
  def test_db
    get "/db"
    assert last_response.ok?
    assert last_response.headers["content-type"] == "application/json"
    last_response
  end
  def test_queries
    get "/queries?queries=10"
    assert last_response.ok?
    assert last_response.headers["content-type"] == "application/json"
    records= Oj.load(last_response.body)
    assert records.size == 10
    get "/queries?queries=boo"
    records= Oj.load(last_response.body)
    assert records.size == 1
    get "/queries?queries=600"
    records= Oj.load(last_response.body)
    assert records.size == 500
  end

end
