# frozen_string_literal: true
require 'bundler/setup'
require 'time'

MAX_PK = 10_000
ID_RANGE = (1..MAX_PK).freeze
ALL_IDS = ID_RANGE.to_a
QUERIES_MIN = 1
QUERIES_MAX = 500
SERVER_STRING = "Sinatra"

Bundler.require(:default) # Load core modules

def connect(dbtype)
  Bundler.require(dbtype) # Load database-specific modules

  opts = {
    :adapter=>(dbtype == :mysql ? 'mysql2' : 'postgresql'),
    :username=>'benchmarkdbuser',
    :password=>'benchmarkdbpass',
    :host=>'tfb-database',
    :database=>'hello_world'
  }

  # Determine threading/thread pool size and timeout
  if defined?(JRUBY_VERSION)
    opts[:pool] = (2 * Math.log(Integer(ENV.fetch('MAX_CONCURRENCY')))).floor
    opts[:checkout_timeout] = 10
  elsif defined?(Puma) && (threads = Puma.cli_config.options.fetch(:max_threads)) > 1
    opts[:pool] = (2 * Math.log(threads)).floor
    opts[:checkout_timeout] = 10
  else
    # TODO: ActiveRecord doesn't have a single-threaded mode?
    opts[:pool] = 1
    opts[:checkout_timeout] = 0
  end

  ActiveRecord::Base.establish_connection(opts)
end

connect ENV.fetch('DBTYPE').to_sym

# Define ORM models
class World < ActiveRecord::Base
  self.table_name = name

  alias_attribute(:randomNumber, :randomnumber) \
    if connection.adapter_name.downcase.start_with?('postgres')

  if connection.adapter_name.downcase.start_with?('mysql')
    def self.upsert_all(attributes, on_duplicate: :update, update_only: nil, returning: nil, unique_by: nil, record_timestamps: nil)
      # On MySQL Batch updates verification isn't supported yet by TechEmpower.
      # https://github.com/TechEmpower/FrameworkBenchmarks/issues/5983
      attributes.each do |attrs|
        where(id: attrs[:id]).update_all(randomNumber: attrs[:randomNumber])
      end
    end
  end
end

class Fortune < ActiveRecord::Base
  self.table_name = name
end

ActiveRecord::Base.connection_handler.clear_active_connections!
