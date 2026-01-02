# frozen_string_literal: true
require 'bundler/setup'
Bundler.require(:default) # Load core modules

require_relative 'db'
require 'time'

# Our Rack application to be executed by rackup
class HelloWorld < Sinatra::Base
  MAX_PK = 10_000
  ID_RANGE = (1..MAX_PK).freeze
  ALL_IDS = ID_RANGE.to_a
  QUERIES_MIN = 1
  QUERIES_MAX = 500

  SERVER_STRING = 'Sinatra'

  configure do
    # Static file serving is ostensibly disabled in modular mode but Sinatra
    # still calls an expensive Proc on every request...
    disable :static

    # XSS, CSRF, IP spoofing, etc. protection are not explicitly required
    disable :protection

    # disable host_authorization for all environments
    set :host_authorization, { permitted_hosts: [] }

    # Only add the charset parameter to specific content types per the requirements
    set :add_charset, [mime_type(:html)]
  end

  helpers do
    def bounded_queries
      queries = params[:queries].to_i
      queries.clamp(QUERIES_MIN, QUERIES_MAX)
    end

    def json(data)
      content_type :json
      data.to_json
    end

    # Return a random number between 1 and MAX_PK
    def rand1
      rand(MAX_PK).succ
    end
  end

  if defined?(Puma)
    after do
      response['Server'] = SERVER_STRING
      response['Date'] = Time.now.httpdate
    end
  else
    after do
      response['Server'] = SERVER_STRING
    end
  end

  # Test type 1: JSON serialization
  get '/json' do
     json :message=>'Hello, World!'
  end

  # Test type 2: Single database query
  get '/db' do
    json World.with_pk(rand1).values
  end

  # Test type 3: Multiple database queries
  get '/queries' do
    ids = ALL_IDS.sample(bounded_queries)
    worlds =
      DB.synchronize do
        ids.map do |id|
          World.with_pk(id)
        end
      end

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

    erb :fortunes, :layout=>true
  end

  # Test type 5: Database updates
  get '/updates' do
    worlds = nil
    ids = ALL_IDS.sample(bounded_queries)
    DB.synchronize do
      worlds =
        ids.map do |id|
          world = World.with_pk(id)
          new_value = rand1
          new_value = rand1 while new_value == world.randomnumber
          world.randomnumber = new_value
          world
        end
      World.batch_update(worlds)
    end

    json worlds.map!(&:values)
  end

  # Test type 6: Plaintext
  get '/plaintext' do
    content_type :text
    'Hello, World!'
  end
end
