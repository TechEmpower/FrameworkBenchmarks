Rage.configure do
  config.server.workers_count = -1
  config.logger = ActiveRecord::Base.logger = Rage::Logger.new(STDOUT)
end
