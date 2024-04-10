# frozen_string_literal: true
require "bundler/setup"
require "time"
require "oj"
MAX_PK = 10_000
QUERY_RANGE = (1..MAX_PK).freeze
ALL_IDS = QUERY_RANGE.to_a
QUERIES_MIN = 1
QUERIES_MAX = 500
SEQUEL_NO_ASSOCIATIONS = true

# Use the OJ gem instead of the JSON one
Oj.mimic_JSON()

SERVER_STRING =
  if defined?(PhusionPassenger)
    [
      PhusionPassenger::SharedConstants::SERVER_TOKEN_NAME,
      PhusionPassenger::VERSION_STRING
    ].join("/").freeze
  elsif defined?(Puma)
    Puma::Const::PUMA_SERVER_STRING
  elsif defined?(Unicorn)
    Unicorn::HttpParser::DEFAULTS["SERVER_SOFTWARE"]
  end

Bundler.require(:default) # Load core modules

def connect(dbtype)
  Bundler.require(dbtype) # Load database-specific modules

  adapters = {
    mysql: {
      mri: "mysql2"
    },
    postgresql: {
      mri: "postgres"
    }
  }

  opts = {}

  # Determine threading/thread pool size and timeout
  if defined?(Puma) &&
        (threads = Puma.cli_config.options.fetch(:max_threads)) > 1
    opts[:max_connections] = (2 * Math.log(threads)).floor
    opts[:pool_timeout] = 10
  else
    Sequel.single_threaded = true
  end

  Sequel.connect "%{adapter}://%{host}/%{database}?user=%{user}&password=%{password}" %
                   {
                     adapter:
                       adapters.fetch(dbtype).fetch(:mri),
                     host: "tfb-database",
                     database: "hello_world",
                     user: "benchmarkdbuser",
                     password: "benchmarkdbpass"
                   },
                 opts
end

DB = connect ENV.fetch("DBTYPE").to_sym

# Define ORM models
class World < Sequel.Model(:World)
  def_column_alias(:randomnumber, :randomNumber) if DB.database_type == :mysql
end

class Fortune < Sequel.Model(:Fortune)
  # Allow setting id to zero (0) per benchmark requirements
  unrestrict_primary_key
end

[World, Fortune].each(&:freeze)
DB.freeze
