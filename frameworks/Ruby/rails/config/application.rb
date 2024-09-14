require_relative "boot"

require "rails"
# Pick the frameworks you want:
require "active_model/railtie"
# require "active_job/railtie"
require "active_record/railtie"
# require "active_storage/engine"
require "action_controller/railtie"
# require "action_mailer/railtie"
# require "action_mailbox/engine"
# require "action_text/engine"
require "action_view/railtie"
# require "action_cable/engine"
# require "rails/test_unit/railtie"

# Require the gems listed in Gemfile, including any gems
# you've limited to :test, :development, or :production.
Bundler.require(*Rails.groups)

module Hello
  class Application < Rails::Application
    # Initialize configuration defaults for originally generated Rails version.
    config.load_defaults 7.2

    # Please, add to the `ignore` list any other `lib` subdirectories that do
    # not contain `.rb` files, or that should not be reloaded or eager loaded.
    # Common ones are `templates`, `generators`, or `middleware`, for example.
    config.autoload_lib(ignore: %w[assets tasks])

    config.action_dispatch.default_headers.merge!('Server' => 'WebServer')

    config.middleware.delete ActionDispatch::Callbacks
    config.middleware.delete ActionDispatch::ContentSecurityPolicy::Middleware
    config.middleware.delete ActionDispatch::Cookies
    config.middleware.delete ActionDispatch::DebugExceptions
    config.middleware.delete ActionDispatch::Executor
    config.middleware.delete ActionDispatch::Flash
    config.middleware.delete ActionDispatch::PermissionsPolicy::Middleware
    config.middleware.delete ActionDispatch::Reloader
    config.middleware.delete ActionDispatch::RemoteIp
    config.middleware.delete ActionDispatch::RequestId
    config.middleware.delete ActionDispatch::Session::CookieStore
    config.middleware.delete ActionDispatch::ShowExceptions
    config.middleware.delete ActiveRecord::Migration::CheckPending
    config.middleware.delete Rack::ConditionalGet
    config.middleware.delete Rack::ETag
    config.middleware.delete Rack::Head
    config.middleware.delete Rack::MethodOverride
    config.middleware.delete Rack::Runtime
    config.middleware.delete Rack::Sendfile
    config.middleware.delete Rack::TempfileReaper
    config.middleware.delete Rails::Rack::Logger
  end
end
