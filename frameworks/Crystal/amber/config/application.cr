Amber::Server.instance.config do |app|
  app.name = "Amber web application."
  app.port = 3000
  app.env = "production"
end
