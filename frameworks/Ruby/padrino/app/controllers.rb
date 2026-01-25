QUERY_RANGE = (1..10_000).freeze
ALL_IDS = QUERY_RANGE.to_a

HelloWorld::App.controllers  do

  after do
    response['Server'] = 'padrino'
  end

  get '/json', :provides => [:json] do
    {message: "Hello, World!"}.to_json
  end

  get '/db', :provides => [:json] do
    world = ActiveRecord::Base.with_connection do
      World.find(rand1).attributes
    end
    world.to_json
  end

  get '/queries', :provides => [:json] do
    worlds = ActiveRecord::Base.with_connection do
      ALL_IDS.sample(bounded_queries).map do |id|
        World.find(id).attributes
      end
    end
    worlds.to_json
  end

  get '/fortunes' do
    @fortunes = Fortune.all.to_a
    @fortunes << Fortune.new(
      id: 0,
      message: "Additional fortune added at request time."
    )
    @fortunes = @fortunes.sort_by(&:message)

    render 'fortunes', layout: "layout"
  end

  get '/updates', :provides => [:json] do
    worlds = []
    ActiveRecord::Base.with_connection do
      worlds = ALL_IDS.sample(bounded_queries).map do |id|
        world = World.find(id)
        new_value = rand1
        new_value = rand1 while new_value == world.randomNumber
        { id: id, randomNumber: new_value }
      end
      worlds.sort_by!{_1[:id]}
      World.upsert_all(worlds)
    end

    worlds.to_json
  end

  get '/plaintext' do
    content_type 'text/plain'
    "Hello, World!"
  end
end
