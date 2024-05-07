# frozen_string_literal: true

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
      queries.clamp(QUERIES_MIN, QUERIES_MAX)
    end

    def json(data)
      content_type :json
      JSON.fast_generate(data)
    end

    # Return a random number between 1 and MAX_PK
    def rand1
      rand(MAX_PK).succ
    end
  end

  after do
    response['Date'] = Time.now.httpdate
  end

  after do
    response['Server'] = SERVER_STRING
  end if SERVER_STRING

  after do
    ActiveRecord::Base.connection_handler.clear_active_connections!
  end

  # Test type 1: JSON serialization
  get '/json' do
     json :message=>'Hello, World!'
  end

  # Test type 2: Single database query
  get '/db' do
    world =
      ActiveRecord::Base.connection_pool.with_connection do
        World.find(rand1).attributes
      end

    json world
  end

  # Test type 3: Multiple database queries
  get '/queries' do
    worlds =
      ActiveRecord::Base.connection_pool.with_connection do
        Array.new(bounded_queries) do
          World.find(rand1)
        end
      end

    json worlds.map!(&:attributes)
  end

  # Test type 4: Fortunes
  get '/fortunes' do
    @fortunes = ActiveRecord::Base.connection_pool.with_connection do
      Fortune.all
    end.to_a
    @fortunes << Fortune.new(
      :id=>0,
      :message=>'Additional fortune added at request time.'
    )
    @fortunes.sort_by!(&:message)

    erb :fortunes, :layout=>true
  end

  # Test type 5: Database updates
  get '/updates' do
    worlds =
      ActiveRecord::Base.connection_pool.with_connection do
        Array.new(bounded_queries) do
          world = World.find(rand1)
          new_value = rand1
          new_value = rand1 while new_value == world.randomnumber
          world.update_columns(randomnumber: new_value)
          world
        end
      end

    json worlds.map!(&:attributes)
  end

  # Test type 6: Plaintext
  get '/plaintext' do
    content_type :text
    'Hello, World!'
  end
end
