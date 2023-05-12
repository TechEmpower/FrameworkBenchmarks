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

 attr :connection
  def initialize(connection_string=nil)
    default_connection_string =
      if RUBY_PLATFORM == "java"
        "jdbc:postgresql://fb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass"
      else
        "postgresql://fb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass"
      end
    url=connection_string ||  default_connection_string
   @connection = Sequel.connect(url, max_connections: 512)
   @connection.extension :async_thread_pool
    prepare_statements
  end

  def prepare_statements
   @world_select=@connection["SELECT id, randomNumber FROM World WHERE id = ?", :$id].prepare(:select, :select_by_id)
   @world_update=@connection["UPDATE World SET randomNumber = ? WHERE id = ?", :$random_number, :$id].prepare(:update, :update_by_id)
   @world_update_returning=@connection["UPDATE World SET randomNumber = ? WHERE id = ? returning *", :$random_number, :$id].prepare(:select, :update_by_id)
   @fortune_select=@connection["SELECT id, message FROM Fortune"].prepare(:select, :select_all)
  end

  def get_random_record
    @world_select.call(id: random_id)[0]
  end

  def get_one_record(id)
    @world_select.call(id: id)[0]
  end

  def get_promises(queries)
    queries = queries.to_i
    queries = if queries < MIN_QUERIES
      MIN_QUERIES
    elsif queries > MAX_QUERIES
      MAX_QUERIES
    else
      queries
    end

    promises = []
    queries.times do
      promises << @connection["SELECT id, randomNumber FROM World WHERE id = ?", random_id].async.first
    end
    return promises
  end

  def get_multiple_records_async(queries)
    promises = get_promises(queries)
    results=[]

    promises.each do | p|
      results << p.to_hash
    end
    return results
  end

  def update_multiple_records_async(queries)
    promises = get_promises(queries)
    results=[]
    update_statements = String.new

    promises.each do | p|
      result =  p.to_hash
      result[:randomnumber]=random_id
      results << result
      update_statements << "UPDATE World SET randomNumber = #{result[:randomnumber]} WHERE id = #{result[:id]};\n"
    end
    @connection.run(update_statements)
    return results
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
