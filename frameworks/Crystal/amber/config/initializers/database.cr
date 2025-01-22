require "granite/adapter/pg"

cpu_count = System.cpu_count - 1 # Always leave 1 core for the system
pool_size = cpu_count * 4 # 4x the number of cores, should be plenty of room for concurrency
database_url = ENV["DATABASE_URL"]
url = "#{database_url}?initial_pool_size=#{pool_size}&max_idle_pool_size=#{pool_size}"

puts url

Granite::Connections << Granite::Adapter::Pg.new(name: "pg", url: url)
