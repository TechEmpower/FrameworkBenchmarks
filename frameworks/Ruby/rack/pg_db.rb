# frozen_string_literal: true

require 'sequel'

if RUBY_PLATFORM == 'java'
  require 'jdbc/postgres'
  Jdbc::Postgres.load_driver
end



class PgDb
  QUERY_RANGE = (1..10_000).freeze # range of IDs in the Fortune DB
  ALL_IDS = QUERY_RANGE.to_a # enumeration of all the IDs in fortune DB
  MIN_QUERIES = 1 # min number of records that can be retrieved
  MAX_QUERIES = 500 # max number of records that can be retrieved

  attr_reader :connection

  def initialize(connection_string = nil, max_connections = 512)
    @connection = Sequel.connect(connection_string, max_connections: max_connections, sql_log_level: :warning)
    if defined?(Falcon)
      Sequel.extension :fiber_concurrency if defined?(Falcon)
    else
      @connection.extension :async_thread_pool
    end

    prepare_statements
  end

  def prepare_statements
    @world_select = @connection['SELECT id, randomNumber FROM World WHERE id = ?', :$id].prepare(:select, :select_by_id)
    @world_update = @connection['UPDATE World SET randomNumber = ? WHERE id = ?', :$random_number, :$id].prepare(:update,
                                                                                                                 :update_by_id)

    @fortune_select = @connection['SELECT id, message FROM Fortune'].prepare(:select, :select_all)
  end

  def select_random_world
    @world_select.call(id: random_id)[0]
  end

  def select_world(id)
    @world_select.call(id: id)[0]
  end

  def validate_count(count)
    count = count.to_i
    if count < MIN_QUERIES
      MIN_QUERIES
    elsif count > MAX_QUERIES
      MAX_QUERIES
    else
      count
    end
  end

  def select_promises(count)
    count = validate_count(count)
    promises = []
    count.times do
      @connection.synchronize do
        promises << @connection['SELECT id, randomNumber FROM World WHERE id = ?', random_id].async.first
      end
    end
    promises
  end

  def select_random_numbers(count)
    count = validate_count(count)
    results = []
    count.times do
      results << @world_random_select.call(randomvalue: random_id, id: random_id)[0]
    end
    results
  end

  def select_worlds(count)
    count = validate_count(count)
    results = []
    count.times do
      results << @world_select.call(id: random_id)[0]
    end
    results
  end

  def select_worlds_async(count)
    promises = select_promises(count)
    results = []
    promises.each do |p|
      results << p.to_hash
    end
    results
  end

  def update_worlds(count, async = false)
    results = if async
      select_worlds_async(count)
    else
      select_worlds(count)
    end
    #values = []
    ids=[]
    sql = String.new("UPDATE world SET randomnumber = CASE id ")
    results.each do |r|
      r[:randomnumber] = random_id
      ids << r[:id]
      sql << "when #{r[:id]} then #{r[:randomnumber]} "
    end
    sql << "ELSE randomnumber END WHERE id IN ( #{ids.join(',')})"
    @connection[sql].update
    results
  end
  def select_fortunes
    @fortune_select.call
  end

  def random_id
    Random.rand(QUERY_RANGE)
  end
end
