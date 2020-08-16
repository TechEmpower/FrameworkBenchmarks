require "amber"
require "granite/adapter/pg"

Granite::Connections << Granite::Adapter::Pg.new(name: "pg", url: Amber.settings.database_url)
