HelloWorld::App.controllers  do
  get '/json', :provides => [:json] do
    response.headers['Server'] = 'padrino'
    response.headers['Date'] = Time.now.httpdate
    {message: "Hello, World!"}.to_json
  end

  get '/db', :provides => [:json] do
    response.headers['Server'] = 'padrino'
    response.headers['Date'] = Time.now.httpdate
    id = Random.rand(10000) + 1
    World.get(id).attributes.to_json
  end

  get '/queries', :provides => [:json] do
    response.headers['Server'] = 'padrino'
    response.headers['Date'] = Time.now.httpdate
    queries = params['queries'].to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    results = (1..queries).map do
      World.get(Random.rand(10000) + 1).attributes
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
    queries = params['queries'].to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    worlds = (1..queries).map do
      # get a random row from the database, which we know has 10000
      # rows with ids 1 - 10000
      world = World.get(Random.rand(10000) + 1)
      world.update(:randomNumber => Random.rand(10000) + 1)
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

