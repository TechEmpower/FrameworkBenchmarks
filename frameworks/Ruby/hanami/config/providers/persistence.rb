Hanami.app.register_provider :persistence, namespace: true do
  prepare do
    require "rom"

    require_relative '../auto_tune'
    num_workers, num_threads = auto_tune

    opts = {
      max_connections: 3,
      pool_timeout: 10
    }
    config = ROM::Configuration.new(:sql, target["settings"].database_url, opts)

    register "config", config
    register "db", config.gateways[:default].connection
  end

  start do
    config = target["persistence.config"]

    config.auto_registration(
      target.root.join("lib/hello_world/persistence"),
      namespace: "HelloWorld::Persistence"
    )

    register "rom", ROM.container(config)
  end

  stop do
    target["persistence.rom"].disconnect
  end
end
