#!/usr/bin/env ruby
require "test/unit"
require "rack"
require "rack/test"
require "oj"
require_relative "pg_db.rb"

ROUTER_APP = Rack::Builder.parse_file("config.ru")

class TestApp < Test::Unit::TestCase
  include Rack::Test::Methods

  def app

    ROUTER_APP
  end

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
  def test_updates
    get "/updates?queries=10"
    assert last_response.ok?
    assert last_response.headers["content-type"] == "application/json"
    records= Oj.load(last_response.body)

    db=PgDb.new
    assert records.size == 10
    records.each do | r|
      pp r
        id = r["id"]
        num = r["randomnumber"]
        record = db.get_one_record(id)
        pp record
        assert record[:randomnumber] == num

    end
  end
  end
