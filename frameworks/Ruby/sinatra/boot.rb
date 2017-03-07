# frozen_string_literal: true
require 'bundler'
require 'time'

MAX_PK = 10_000
QUERIES_MIN = 1
QUERIES_MAX = 500

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

Bundler.require(:default) # Load core modules

def connect(dbtype)
  Bundler.require(dbtype) # Load database-specific modules

  opts = {
    :adapter=>(dbtype == :mysql ? 'mysql2' : 'postgresql'),
    :username=>'benchmarkdbuser',
    :password=>'benchmarkdbpass',
    :host=>ENV.fetch('DBHOST', '127.0.0.1'),
    :database=>'hello_world'
  }

  # Determine threading/thread pool size and timeout
  if defined?(JRUBY_VERSION)
    opts[:pool] = Integer(ENV.fetch('MAX_CONCURRENCY'))
    opts[:check_timeout] = 10
  elsif defined?(Puma)
    opts[:pool] = Puma.cli_config.options.fetch(:max_threads)
    opts[:check_timeout] = 10
  else
    # TODO: ActiveRecord doesn't have a single-threaded mode?
    opts[:pool] = 1
    opts[:check_timeout] = 0
  end

  ActiveRecord::Base.establish_connection(opts)
end

connect ENV.fetch('DBTYPE').to_sym

# Define ORM models
class World < ActiveRecord::Base
  self.table_name = name

  alias_attribute(:randomnumber, :randomNumber) \
    if ActiveRecord::Base.connection.adapter_name.downcase.start_with?('mysql')
end

class Fortune < ActiveRecord::Base
  self.table_name = name
end

ActiveRecord::Base.clear_active_connections!
