require "../src/controllers/test_controller"

Kemalyst::Application.config do |config|
  # Set the binding host ip address.  Defaults to "0.0.0.0"
  # config.host = "0.0.0.0"

  # Set the port.  Defaults to 3000.
  # config.port = 3000

  # Configure reuse_port option
  config.reuse_port = true

  # Disable unused middleware
  config.handlers = [] of HTTP::Handler
  config.handlers << Crack::Handler::Error.instance
  config.handlers << Crack::Handler::Params.instance
  config.handlers << TestController.instance
  config.handlers << Kemalyst::Handler::Router.instance
end
