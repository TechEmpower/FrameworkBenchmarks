# frozen_string_literal: true
require 'bundler/setup'
require 'time'

MAX_PK = 10_000
ID_RANGE = (1..10_000).freeze
ALL_IDS = ID_RANGE.to_a
QUERIES_MIN = 1
QUERIES_MAX = 500
SEQUEL_NO_ASSOCIATIONS = true
CONTENT_TYPE = 'Content-Type'
JSON_TYPE = 'application/json'
HTML_TYPE = 'text/html; charset=utf-8'
PLAINTEXT_TYPE = 'text/plain'
DATE_HEADER = 'Date'
SERVER_HEADER = 'Server'

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

  adapters = {
    mysql: { jruby: 'jdbc:mysql', mri: 'mysql2' },
    postgresql: { jruby: 'jdbc:postgresql', mri: 'postgres' }
  }

  opts = {}

  # Determine threading/thread pool size and timeout
  if defined?(JRUBY_VERSION)
    opts[:max_connections] = (2 * Math.log(Integer(ENV.fetch('MAX_CONCURRENCY')))).floor
    opts[:pool_timeout] = 10
  elsif defined?(Puma) && (threads = Puma.cli_config.options.fetch(:max_threads)) > 1
    opts[:max_connections] = (2 * Math.log(threads)).floor
    opts[:pool_timeout] = 10
  else
    Sequel.single_threaded = true
  end

  Sequel.connect \
    '%{adapter}://%{host}/%{database}?user=%{user}&password=%{password}' % {
      adapter: adapters.fetch(dbtype).fetch(defined?(JRUBY_VERSION) ? :jruby : :mri),
      host: 'tfb-database',
      database: 'hello_world',
      user: 'benchmarkdbuser',
      password: 'benchmarkdbpass'
    }, opts
end

DB = connect ENV.fetch('DBTYPE').to_sym

# Define ORM models
class World < Sequel::Model(:World)
  BY_ID = naked.where(id: :$id).prepare(:first, :world_by_id)
  UPDATE = where(id: :$id).prepare(:update, :world_update, randomnumber: :$randomnumber)

  def_column_alias(:randomnumber, :randomNumber) if DB.database_type == :mysql

  def self.batch_update(worlds)
    if DB.database_type == :mysql
      worlds.each do |world|
        UPDATE.(id: world[:id], randomnumber: world[:randomnumber])
      end
    else
      ids = []
      sql = String.new("UPDATE world SET randomnumber = CASE id ")
      worlds.each do |world|
        sql << "when #{world[:id]} then #{world[:randomnumber]} "
        ids << world[:id]
      end
      sql << "ELSE randomnumber END WHERE id IN ( #{ids.join(',')})"
      DB.run(sql)
    end
  end
end

class Fortune < Sequel::Model(:Fortune)
  # Allow setting id to zero (0) per benchmark requirements
  unrestrict_primary_key
end

[World, Fortune].each(&:freeze)
DB.freeze
