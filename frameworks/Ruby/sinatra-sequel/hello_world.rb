MAX_PK = 10_000
SEQUEL_NO_ASSOCIATIONS = true

Bundler.require :default

# Configure Slim templating engine
Slim::Engine.set_options \
  :format => :html,
  :sort_attrs => false

# Configure Sequel ORM
DB = Sequel.connect \
  :adapter => RUBY_PLATFORM == 'java' ? 'jdbc:mysql' : 'mysql2',
  :host => ENV['DB_HOST'],
  :database => 'hello_world',
  :user => 'benchmarkdbuser',
  :password => 'benchmarkdbpass',
  :max_connections => 32, # == max worker threads per process
  :pool_timeout => 5

# Allow #to_json on models and arrays of models
Sequel::Model.plugin :json_serializer

class World < Sequel::Model(:World); end

class Fortune < Sequel::Model(:Fortune)
  # Allow setting id to zero (0) per benchmark requirements
  unrestrict_primary_key
end

# Our Rack application to be executed by rackup
class HelloWorld < Sinatra::Base
  configure do
    disable :protection

    # Don't add ;charset= to any content types per the benchmark requirements
    set :add_charset, []

    # Specify the encoder - otherwise, sinatra/json inefficiently
    # attempts to load one of several on each request
    set :json_encoder, :to_json
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
    response['Server'] = 'Puma'
    response['Date'] = Time.now.to_s
  end

  get '/json' do
    json :message => 'Hello, World!'
  end

  get '/plaintext' do
    content_type 'text/plain'
    'Hello, World!'
  end

  get '/db' do
    json World[rand1]
  end

  get '/queries' do
    queries = (params[:queries] || 1).to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    json World.where(:id => randn(queries))
  end

  get '/fortunes' do
    @fortunes = Fortune.all
    @fortunes << Fortune.new(
      :id => 0,
      :message => 'Additional fortune added at request time.'
    )
    @fortunes.sort_by!(&:message)

    slim :fortunes
  end

  get '/updates' do
    queries = (params[:queries] || 1).to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    # Prepare our updates in advance so transaction retries are idempotent
    updates = randn(queries).sort!.map! { |id| [id, rand1] }

    worlds = nil

    World.db.transaction do
      worlds = World
        .where(:id => updates.transpose.first)
        .for_update

      World.dataset
        .on_duplicate_key_update(:randomNumber)
        .import([:id, :randomNumber], updates)
    end

    json worlds
  end
end
