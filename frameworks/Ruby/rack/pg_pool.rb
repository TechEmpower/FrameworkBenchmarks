# frozen_string_literal: true
class PgPool
  QUERY_RANGE = 1..10_000 # range of IDs in the Fortune DB
  ALL_IDS = QUERY_RANGE.to_a # enumeration of all the IDs in fortune DB
  MIN_QUERIES = 1 # min number of records that can be retrieved
  MAX_QUERIES = 500 # max number of records that can be retrieved
  WORLD_SELECT = "SELECT id, randomNumber FROM World WHERE id = $1"
  WORLD_UPDATE = "UPDATE World SET randomNumber = $1 WHERE id = $2"
  FORTUNE_SELECT = "SELECT id, message FROM Fortune"

  attr pool

  def initialize(database_url, pool_size = 512, timeout = 10)
    @pool =
      ConnectionPool.new(size: 256, timeout: 5) do
        PG::Connection.new(database_url)
      end
    fill_pool
  end
  def random_id
    Random.rand(QUERY_RANGE)
  end

end
