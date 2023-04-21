class PgPool
  attr pool
  def initialize(database_url, pool_size = 512, timeout = 10)
    @pool =
      ConnectionPool.new(size: 256, timeout: 5) do
        PG::Connection.new(database_url)
      end
    fill_pool
  end
end
