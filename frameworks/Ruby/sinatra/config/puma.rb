before_fork do
  ActiveRecord::Base.connection_handler.clear_active_connections!
end
