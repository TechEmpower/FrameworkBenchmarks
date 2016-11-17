# frozen_string_literal: true
require 'time'

MAX_PK = 10_000
QUERIES_MIN = 1
QUERIES_MAX = 500
SEQUEL_NO_ASSOCIATIONS = true

Bundler.require(:default) # Load core modules

def connect(dbtype)
  Bundler.require(dbtype) # Load database-specific modules

  adapters = {
    :mysql=>{ :jruby=>'jdbc:mysql', :mri=>'mysql2' },
    :postgresql=>{ :jruby=>'jdbc:postgresql', :mri=>'postgres' }
  }

  Sequel.connect \
    '%<adapter>s://%<host>s/%<database>s?user=%<user>s&password=%<password>s' % {
      :adapter=>adapters.fetch(dbtype).fetch(defined?(JRUBY_VERSION) ? :jruby : :mri),
      :host=>ENV.fetch('DBHOST', '127.0.0.1'),
      :database=>'hello_world',
      :user=>'benchmarkdbuser',
      :password=>'benchmarkdbpass'
    },
    :max_connections=>Puma.cli_config.options.fetch(:max_threads),
    :pool_timeout=>10
end

DB = connect ENV.fetch('DBTYPE').to_sym

# Define ORM models
class World < Sequel::Model(:World)
  def_column_alias(:randomnumber, :randomNumber) if DB.database_type == :mysql
end

class Fortune < Sequel::Model(:Fortune)
  # Allow setting id to zero (0) per benchmark requirements
  unrestrict_primary_key
end

# Configure Slim templating engine
Slim::Engine.set_options \
  :format=>:html,
  :sort_attrs=>false

# Our Rack application to be executed by rackup
class HelloWorld < Sinatra::Base
  configure do
    # Static file serving is ostensibly disabled in modular mode but Sinatra
    # still calls an expensive Proc on every request...
    disable :static

    # XSS, CSRF, IP spoofing, etc. protection are not explicitly required
    disable :protection

    # Only add the charset parameter to specific content types per the requirements
    set :add_charset, [mime_type(:html)]
  end

  helpers do
    def bounded_queries
      queries = params[:queries].to_i
      return QUERIES_MIN if queries < QUERIES_MIN
      return QUERIES_MAX if queries > QUERIES_MAX
      queries
    end

    def json(data)
      content_type :json
      JSON.fast_generate(data)
    end

    # Return a random number between 1 and MAX_PK
    def rand1
      Random.rand(MAX_PK).succ
    end

    # Return an array of `n' unique random numbers between 1 and MAX_PK
    def randn(n)
      (1..MAX_PK).to_a.shuffle!.take(n)
    end
  end

  after do
    # Add mandatory HTTP headers to every response
    response['Server'] ||= Puma::Const::PUMA_SERVER_STRING
    response['Date'] ||= Time.now.httpdate
  end

  # Test type 1: JSON serialization
  get '/json' do
     json :message=>'Hello, World!'
  end

  # Test type 2: Single database query
  get '/db' do
    json World[rand1].values
  end

  # Test type 3: Multiple database queries
  get '/queries' do
    # Benchmark requirements explicitly forbid a WHERE..IN here, so be good
    worlds = randn(bounded_queries).map! { |id| World[id] }

    json worlds.map!(&:values)
  end

  # Test type 4: Fortunes
  get '/fortunes' do
    @fortunes = Fortune.all
    @fortunes << Fortune.new(
      :id=>0,
      :message=>'Additional fortune added at request time.'
    )
    @fortunes.sort_by!(&:message)

    slim :fortunes
  end

  # Test type 5: Database updates
  get '/updates' do
    # Benchmark requirements explicitly forbid a WHERE..IN here, transactions
    # are optional, batch updates are allowed (but each transaction can only
    # read and write a single record?), so... be good
    worlds = randn(bounded_queries).map! do |id|
      DB.transaction do
        world = World.for_update[id]
        world.update :randomnumber=>rand1
        world
      end
    end

    json worlds.map!(&:values)
  end

  # Test type 6: Plaintext
  get '/plaintext' do
    content_type :text
    'Hello, World!'
  end
end
