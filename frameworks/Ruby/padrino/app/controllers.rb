MAX_PK = 10_000
QUERIES_MIN = 1
QUERIES_MAX = 500

HelloWorld::App.controllers  do
  get '/json', :provides => [:json] do
    response.headers['Server'] = 'padrino'
    response.headers['Date'] = Time.now.httpdate
    {message: "Hello, World!"}.to_json
  end

  get '/db', :provides => [:json] do
    response.headers['Server'] = 'padrino'
    response.headers['Date'] = Time.now.httpdate
    id = Random.rand(MAX_PK) + 1
    World.get(id).attributes.to_json
  end

  get '/queries', :provides => [:json] do
    response.headers['Server'] = 'padrino'
    response.headers['Date'] = Time.now.httpdate
    queries = params['queries'].to_i.clamp(QUERIES_MIN, QUERIES_MAX)

    results = (1..queries).map do
      World.get(Random.rand(MAX_PK) + 1).attributes
    end.to_json
  end

  get '/fortunes' do
    response.headers['Server'] = 'padrino'
    response.headers['Date'] = Time.now.httpdate
    @fortunes = Fortune.all
    @fortunes << Fortune.new(:id => 0, :message => "Additional fortune added at request time.")
    @fortunes = @fortunes.sort_by { |x| x.message }

    render 'fortunes', layout: "layout"
  end

  get '/updates', :provides => [:json] do
    response.headers['Server'] = 'padrino'
    response.headers['Date'] = Time.now.httpdate
    queries = params['queries'].to_i.clamp(QUERIES_MIN, QUERIES_MAX)

    worlds = (1..queries).map do
      # get a random row from the database, which we know has 10000
      # rows with ids 1 - 10000
      world = World.get(Random.rand(MAX_PK) + 1)
      world.update(randomNumber: Random.rand(MAX_PK) + 1)
      world.attributes
    end

    worlds.to_json
  end

  get '/plaintext' do
    response.headers['Server'] = 'padrino'
    response.headers['Date'] = Time.now.httpdate
    content_type 'text/plain'
    "Hello, World!"
  end

end
