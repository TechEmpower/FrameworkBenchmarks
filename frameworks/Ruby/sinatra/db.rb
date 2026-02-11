# frozen_string_literal: true

def connect(dbtype)
  Bundler.require(dbtype) # Load database-specific modules

  opts = {
    username: 'benchmarkdbuser',
    password: 'benchmarkdbpass',
    host: 'tfb-database',
    database: 'hello_world'
  }

  if dbtype == :mysql
    opts[:adapter] = 'trilogy'
    opts[:ssl] = true
    opts[:ssl_mode] = 4 # Trilogy::SSL_PREFERRED_NOVERIFY
    opts[:tls_min_version] = 3 # Trilogy::TLS_VERSION_12
  else
    opts[:adapter] = 'postgresql'
  end

  # Determine threading/thread pool size and timeout
  if defined?(Puma)
    opts[:pool] = ENV.fetch('MAX_THREADS')
    opts[:checkout_timeout] = 10
  else
    opts[:pool] = 512
  end

  ActiveRecord::Base.establish_connection(opts)
  ActiveRecord::Base.logger = nil
end

connect ENV.fetch('DBTYPE').to_sym

# Define ORM models
class World < ActiveRecord::Base
  self.table_name = name

  alias_attribute(:randomNumber, :randomnumber) \
    if connection.adapter_name.downcase.start_with?('postgres')

  if connection.adapter_name.downcase.start_with?('trilogy')
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
