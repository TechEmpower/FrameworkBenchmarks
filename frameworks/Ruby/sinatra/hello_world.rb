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

  DATE_HEADER = 'Date'
  SERVER_HEADER = 'Server'
  SERVER_STRING = 'Sinatra'

  configure do
    # Static file serving is ostensibly disabled in modular mode but Sinatra
    # still calls an expensive Proc on every request...
    disable :static

    # XSS, CSRF, IP spoofing, etc. protection are not explicitly required
    disable :protection

    # disable it for all environments
    set :host_authorization, { permitted_hosts: [] }

    # Only add the charset parameter to specific content types per the requirements
    set :add_charset, [mime_type(:html)]

    # Disable logging middleware
    set :logging, nil

    # Set root once instead executing the proc on every request
    set :root, File.expand_path(__dir__)
  end

  # Test type 1: JSON serialization
  get '/json' do
    render_json message: 'Hello, World!'
  end

  # Test type 2: Single database query
  get '/db' do
    world =
      ActiveRecord::Base.with_connection do
        World.find(rand1).attributes
      end

    render_json world
  end

  # Test type 3: Multiple database queries
  get '/queries' do
    ids = ALL_IDS.sample(bounded_queries)
    worlds =
      ActiveRecord::Base.with_connection do
        ids.map do |id|
          World.find(id).attributes
        end
      end

    render_json worlds
  end

  # Test type 4: Fortunes
  get '/fortunes' do
    @fortunes = ActiveRecord::Base.with_connection do
      Fortune.all
    end.to_a
    fortune = Fortune.new
    fortune.id = 0
    fortune.message = "Additional fortune added at request time."
    @fortunes << fortune
    @fortunes.sort_by!(&:message)

    render_html :fortunes
  end

  # Test type 5: Database updates
  get '/updates' do
    worlds = nil
    ids = ALL_IDS.sample(bounded_queries)
    ActiveRecord::Base.with_connection do
      worlds = ids.map do |id|
        world = World.find(id)
        new_value = rand1
        new_value = rand1 until new_value != world.randomNumber
        { id: id, randomNumber: new_value }
      end
    end
    worlds.sort_by!{_1[:id]}
    ActiveRecord::Base.with_connection do
      World.upsert_all(worlds)
    end
    render_json worlds
  end

  # Test type 6: Plaintext
  get '/plaintext' do
    render_text 'Hello, World!'
  end

  private

  def render_json(data)
    add_headers
    content_type :json
    data.to_json
  end

  def render_html(template)
    add_headers
    render :erb, template, layout: true
  end

  def render_text(content)
    add_headers
    content_type :text
    content
  end

  def bounded_queries
    queries = params[:queries].to_i
    queries.clamp(QUERIES_MIN, QUERIES_MAX)
  end

  # Return a random number between 1 and MAX_PK
  def rand1
    rand(MAX_PK).succ
  end

  if defined?(Puma)
    def add_headers
      response[SERVER_HEADER] = SERVER_STRING
      response[DATE_HEADER] = Time.now.httpdate
    end
  else
    def add_headers
      response[SERVER_HEADER] = SERVER_STRING
    end
  end
end
