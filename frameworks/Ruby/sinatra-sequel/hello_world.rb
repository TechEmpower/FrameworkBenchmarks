# frozen_string_literal: true

# Configure Slim templating engine
Slim::Engine.set_options :format=>:html, :sort_attrs=>false

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
      rand(MAX_PK).succ
    end
  end

  after do
    response['Date'] = Time.now.httpdate
  end

  after do
    response['Server'] = SERVER_STRING
  end if SERVER_STRING

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
    worlds =
      DB.synchronize do
        Array.new(bounded_queries) do
          World.with_pk(rand1)
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

    slim :fortunes
  end

  # Test type 5: Database updates
  get '/updates' do
    worlds =
      DB.synchronize do
        Array.new(bounded_queries) do
          world = World.with_pk(rand1)
          world.update(:randomnumber=>rand1)
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
