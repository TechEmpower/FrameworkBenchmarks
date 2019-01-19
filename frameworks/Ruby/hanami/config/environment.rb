require 'bundler/setup'
require 'hanami/setup'
require 'hanami/model'
require_relative '../lib/hello_world'
require_relative '../apps/web/application'

Hanami.configure do
  mount Web::Application, at: '/'

  model do
    ##
    # Database adapter
    #
    # Available options:
    #
    #  * SQL adapter
    #    adapter :sql, 'sqlite://db/hello_world_development.sqlite3'
    #    adapter :sql, 'postgresql://localhost/hello_world_development'
    #    adapter :sql, 'mysql://localhost/hello_world_development'
    #

    host = ENV['DB_HOST'] || 'localhost'
    adapter :sql, "mysql2://benchmarkdbuser:benchmarkdbpass@#{host}/hello_world"
  end

  environment :production do
  end
end
