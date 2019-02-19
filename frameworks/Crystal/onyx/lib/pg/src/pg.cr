require "db"
require "./pg/*"

module PG
  # Establish a connection to the database
  def self.connect(url)
    DB.open(url)
  end

  # Establish a special listen connection to the database
  def self.connect_listen(url, *channels : String, &blk : PQ::Notification ->) : ListenConnection
    ListenConnection.new(url, *channels, &blk)
  end

  class ListenConnection
    @conn : PG::Connection

    def initialize(url, *channels : String, &blk : PQ::Notification ->)
      @conn = DB.connect(url).as(PG::Connection)
      @conn.on_notification(&blk)
      @conn.listen(*channels)
    end

    # Close the connection.
    def close
      @conn.close
    end
  end
end
