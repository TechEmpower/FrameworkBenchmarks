# frozen_string_literal: true

SEQUEL_NO_ASSOCIATIONS = true
Sequel.extension :fiber_concurrency

# Determine thread pool size and timeout
opts = {
  max_connections: 512,
  pool_timeout: 10
}

DB = Sequel.connect \
  '%{adapter}://%{host}/%{database}?user=%{user}&password=%{password}' % {
    :adapter=>'postgres',
    :host=>'tfb-database',
    :database=>'hello_world',
    :user=>'benchmarkdbuser',
    :password=>'benchmarkdbpass'
  }, opts

# Define ORM models
class World < Sequel::Model(:World)
  def self.batch_update(worlds)
    ids = []
    sql = String.new("UPDATE world SET randomnumber = CASE id ")
    worlds.each do |world|
      sql << "when #{world.id} then #{world.randomnumber} "
      ids << world.id
    end
    sql << "ELSE randomnumber END WHERE id IN ( #{ids.join(',')})"
    DB.run(sql)
  end
end

class Fortune < Sequel::Model(:Fortune)
  # Allow setting id to zero (0) per benchmark requirements
  unrestrict_primary_key
end

[World, Fortune].each(&:freeze)
DB.freeze
