require "../src/middleware/custom_headers"

Kemalyst::Application.config do |config|
  # Set the binding host ip address.  Defaults to "0.0.0.0"
  # config.host = "0.0.0.0"

  # Set the port.  Defaults to 3000.
  # config.port = 3000

  # Configure reuse_port option
  config.reuse_port = true

  # Disable unused middleware
  config.handlers.clear
  config.handlers << Kemalyst::Handler::Error.instance
  config.handlers << Kemalyst::Handler::Params.instance
  config.handlers << Middleware::CustomHeaders.instance
  config.handlers << Kemalyst::Handler::Router.instance
end
