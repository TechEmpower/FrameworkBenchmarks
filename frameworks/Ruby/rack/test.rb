#!/usr/bin/env ruby
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
end
