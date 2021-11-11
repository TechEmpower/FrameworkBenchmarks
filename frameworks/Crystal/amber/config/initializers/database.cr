require "granite/adapter/pg"

Granite.settings.logger = Logger.new(nil)
Granite::Adapters << Granite::Adapter::Pg.new({name: "pg", url: ENV["DATABASE_URL"]})
