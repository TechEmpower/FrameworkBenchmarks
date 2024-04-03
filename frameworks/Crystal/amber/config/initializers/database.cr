require "granite/adapter/pg"

cpu_count = System.cpu_count
pool_size = 56 // cpu_count
database_url = ENV["DATABASE_URL"]
url = "#{database_url}?initial_pool_size=#{pool_size}&max_idle_pool_size=#{pool_size}"

puts url

Granite::Connections << Granite::Adapter::Pg.new(name: "pg", url: url)
