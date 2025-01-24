Rage.configure do
  config.server.workers_count = -1
  config.logger = Rage::Logger.new(STDOUT)
end
