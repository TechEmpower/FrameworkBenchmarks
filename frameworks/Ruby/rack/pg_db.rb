# frozen_string_literal: true

require 'sequel'
if RUBY_PLATFORM == "java"
  require "jdbc/postgres"
  Jdbc::Postgres.load_driver
end

class PgDb
  QUERY_RANGE = 1..10_000 # range of IDs in the Fortune DB
  ALL_IDS = QUERY_RANGE.to_a # enumeration of all the IDs in fortune DB
  MIN_QUERIES = 1 # min number of records that can be retrieved
  MAX_QUERIES = 500 # max number of records that can be retrieved


  def initialize()
    default_connection_string =
      if RUBY_PLATFORM == "java"
        "jdbc:postgresql://fb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass"
      else
        "postgresql://postgres/fb-database?user=benchmarkdbuser&password=benchmarkdbpass"
      end
      connection_string=

    @db = Sequel.connect(ENV["CONNECTION_STRING"] || default_connection_string)
    prepare_statements
  end

  def prepare_statements
   @world_select= @db["SELECT id, randomNumber FROM World WHERE id = ?", :$id].prepare(:select, :select_by_id)
   @world_update=@db["UPDATE World SET randomNumber = ? WHERE id = ?", :$random_number, :$id].prepare(:update, :update_by_id)
   @world_update_returning=@db["UPDATE World SET randomNumber = ? WHERE id = ? returning *", :$random_number, :$id].prepare(:select, :update_by_id)
   @fortune_select=@db["SELECT id, message FROM Fortune"].prepare(:select, :select_all)
  end

  def get_one_record
    @world_select.call(id: random_id)
  end
  def update_one_record
    @world_update.call(id: random_id, random_number: random_id)
  end

  def get_fortunes
    @fortune_select.call()
  end

  def random_id
    Random.rand(QUERY_RANGE)
  end
end
