# frozen_string_literal: true

# Our Rack application to be executed by rackup
class HelloWorld < Roda
  plugin :hooks
  plugin :render, escape: true, layout_opts: { cache_key: 'default_layout' }
  plugin :static_routing

  def bounded_queries
    queries = request['queries'].to_i
    return QUERIES_MIN if queries < QUERIES_MIN
    return QUERIES_MAX if queries > QUERIES_MAX

    queries
  end

  # Return a random number between 1 and MAX_PK
  def rand1
    rand(MAX_PK) + 1
  end

  after do
    response['Date'] = Time.now.httpdate
    response['Server'] = SERVER_STRING if SERVER_STRING
  end

  # Test type 1: JSON serialization
  static_get '/json' do |_|
    response['Content-Type'] = 'application/json'

    { message: 'Hello, World!' }.to_json
  end

  # Test type 2: Single database query
  static_get '/db' do |_|
    response['Content-Type'] = 'application/json'

    World.with_pk(rand1).values.to_json
  end

  # Test type 3: Multiple database queries
  static_get '/queries' do |_|
    response['Content-Type'] = 'application/json'
    worlds = DB.synchronize do
      Array.new(bounded_queries) do
        World.with_pk(rand1).values
      end
    end

    worlds.to_json
  end

  # Test type 4: Fortunes
  static_get '/fortunes' do |_|
    response['Content-Type'] = 'text/html; charset=utf-8'
    @fortunes = Fortune.all
    @fortunes << Fortune.new(
      id: 0,
      message: 'Additional fortune added at request time.'
    )
    @fortunes.sort_by!(&:message)

    view :fortunes
  end

  # Test type 5: Database updates
  static_get '/updates' do |_|
    response['Content-Type'] = 'application/json'
    worlds = DB.synchronize do
      Array.new(bounded_queries) do
        world = World.with_pk(rand1)
        new_value = rand1
        new_value = rand1 while new_value == world.randomnumber
        world.update(randomnumber: new_value)
        world.values
      end
    end

    worlds.to_json
  end

  # Test type 6: Plaintext
  static_get '/plaintext' do |_|
    response['Content-Type'] = 'text/plain'

    'Hello, World!'
  end

  # Even though we don't have any non-static routes, this is still required.
  route { |_| }
end
