Hanami.app.register_provider :persistence, namespace: true do
  prepare do
    require "rom"

    require_relative '../auto_tune'
    num_workers, num_threads = auto_tune

    opts = {}

    if (threads = num_threads) > 1
      opts[:max_connections] = (2 * Math.log(threads)).floor
      opts[:pool_timeout] = 10
    end
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
end
