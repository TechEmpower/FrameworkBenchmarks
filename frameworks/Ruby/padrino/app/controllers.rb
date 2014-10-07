HelloWorld::App.controllers  do
  get '/json', :provides => [:js] do
    {message: "Hello, World!"}.to_json
  end

  get '/db', :provides => [:js] do
    id = Random.rand(10000) + 1
    World.get(id).attributes.to_json
  end

  get '/queries', :provides => [:js] do
    queries = params['queries'].to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    results = (1..queries).map do
      World.get(Random.rand(10000) + 1).attributes
    end.to_json
  end

  get '/fortunes' do
    @fortunes = Fortune.all
    @fortunes << Fortune.new(:id => 0, :message => "Additional fortune added at request time.")
    @fortunes = @fortunes.sort_by { |x| x.message }

    render 'fortunes', layout: "layout"
  end

  get '/updates', :provides => [:js] do
    queries = params['queries'].to_i
    queries = 1 if queries < 1
    queries = 500 if queries > 500

    worlds = (1..queries).map do
      world = World.get(Random.rand(10000) + 1)
      world.update(:randomNumber => Random.rand(10000) + 1)
      world.attributes
    end

    #mass update
    values = worlds.map { |h| ['(', h[:id], ',' , h[:randomNumber], ')', ','] }.flatten[0..-2].join
    sql = "INSERT INTO `World` (`id`,`randomNumber`) VALUES #{values} ON DUPLICATE KEY UPDATE `World`.`randomNumber` = VALUES(`randomNumber`)"
    adapter = DataMapper.repository(:default).adapter
    adapter.execute(sql)
    
    worlds.to_json
  end

  get '/plaintext' do
    content_type 'text/plain'
    "Hello, World!"
  end

end
