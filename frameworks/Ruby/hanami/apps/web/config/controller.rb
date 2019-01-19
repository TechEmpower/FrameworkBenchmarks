require 'hanami/controller'

SERVER_STRING =
  if defined?(PhusionPassenger)
    [
      PhusionPassenger::SharedConstants::SERVER_TOKEN_NAME,
      PhusionPassenger::VERSION_STRING
    ].join('/').freeze
  elsif defined?(Puma)
    Puma::Const::PUMA_SERVER_STRING
  elsif defined?(Unicorn)
    Unicorn::HttpParser::DEFAULTS['SERVER_SOFTWARE']
  end

Hanami::Controller.configure do
  default_charset('')
  default_headers({
    'Date'   => Time.now.httpdate,
    'Server' => SERVER_STRING || 'WebServer'
  })
end
