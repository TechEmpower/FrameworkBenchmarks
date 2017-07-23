Amber::Server.instance.config do |app|
  app_path = __FILE__
  app.name = "Amber web application."
  app.port = 3000
  app.env = "production"
end
