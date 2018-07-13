require "./app"

Habitat.raise_if_missing_settings!

app = App.new
puts "Listening on #{app.base_uri}"
app.listen

Signal::INT.trap do
  app.close
end
