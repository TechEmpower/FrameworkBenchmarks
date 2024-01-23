require "./app"

Habitat.raise_if_missing_settings!

app_server = AppServer.new

Signal::INT.trap do
  app_server.close
end

app_server.listen
