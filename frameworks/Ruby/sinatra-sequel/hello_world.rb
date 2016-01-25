MAX_PK = 10_000
SEQUEL_NO_ASSOCIATIONS = true

Bundler.require :default

# Configure Sequel ORM (Sequel::DATABASES)
Sequel.connect \
  '%<adapter>s://%<host>s/%<database>s?user=%<user>s&password=%<password>s' % {
    :adapter => RUBY_PLATFORM == 'java' ? 'jdbc:mysql' : 'mysql2',
    :host => ENV['DBHOST'],
    :database => 'hello_world',
    :user => 'benchmarkdbuser',
    :password => 'benchmarkdbpass'
  },
  :max_connections => (ENV['MAX_THREADS'] || 4).to_i,
  :pool_timeout => 5

# Allow #to_json on models and datasets
Sequel::Model.plugin :json_serializer

class World < Sequel::Model(:World); end

class Fortune < Sequel::Model(:Fortune)
  # Allow setting id to zero (0) per benchmark requirements
  unrestrict_primary_key
end

# Configure Slim templating engine
Slim::Engine.set_options \
  :format => :html,
  :sort_attrs => false

# Our Rack application to be executed by rackup
class HelloWorld < Sinatra::Base
  configure do
    # Static file serving is ostensibly disabled in modular mode but Sinatra
    # still calls an expensive Proc on every request...
    disable :static

    # XSS, CSRF, IP spoofing, etc. protection are not explicitly required
    disable :protection

    # Only add ;charset= to specific content types per the benchmark requirements
    set :add_charset, %w[text/html]
  end

  helpers do
    # Return a random number between 1 and MAX_PK
    def rand1
      Random.rand(MAX_PK) + 1
    end

    # Return an array of `n' unique random numbers between 1 and MAX_PK
    def randn(n)
      (1..MAX_PK).to_a.shuffle!.take(n)
    end
  end

  after do
    # Add mandatory HTTP headers to every response
    response['Server'] ||= 'Puma'.freeze
    response['Date'] ||= Time.now.to_s
  end

  get '/json', :provides => :json do
    JSON.fast_generate :message => 'Hello, World!'.freeze
  end

  get '/plaintext', :provides => :text do
    'Hello, World!'.freeze
  end

  get '/db', :provides => :json do
    World[rand1].to_json
  end

  get '/queries', :provides => :json do
    queries = (params[:queries] || 1).to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    # Benchmark requirements explicitly forbid a WHERE..IN here, so be good
    worlds = randn(queries).map! { |id| World[id] }

    World.to_json :array => worlds
  end

  get '/fortunes' do
    @fortunes = Fortune.all
    @fortunes << Fortune.new(
      :id => 0,
      :message => 'Additional fortune added at request time.'.freeze
    )
    @fortunes.sort_by!(&:message)

    slim :fortunes
  end

  get '/updates', :provides => :json do
    queries = (params[:queries] || 1).to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    # Benchmark requirements explicitly forbid a WHERE..IN here, and specify
    # that each transaction only read and write a single record, so be good
    worlds = []
    randn(queries).each do |id|
      World.db.transaction do
        world = World.for_update[id]
        world.update :randomNumber => rand1
        worlds << world
      end
    end

    World.to_json :array => worlds
  end
end
