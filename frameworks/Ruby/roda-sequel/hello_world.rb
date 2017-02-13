# frozen_string_literal: true

# Our Rack application to be executed by rackup
class HelloWorld < Roda
  plugin :default_headers, 'Content-Type'=>'text/html; charset=utf-8'
  plugin :default_headers, 'Server'=>SERVER_STRING if SERVER_STRING
  plugin :json
  plugin :render, :escape=>:erubi, :layout_opts=>{ :cache_key=>'default_layout' }
  plugin :static_routing

  def bounded_queries
    queries = request['queries'].to_i
    return QUERIES_MIN if queries < QUERIES_MIN
    return QUERIES_MAX if queries > QUERIES_MAX
    queries
  end

  # Return a random number between 1 and MAX_PK
  def rand1
    Random.rand(MAX_PK).succ
  end

  # Return an array of `n' unique random numbers between 1 and MAX_PK
  def randn(n)
    (1..MAX_PK).to_a.shuffle!.take(n)
  end

  # Test type 1: JSON serialization
  static_get '/json' do
    response['Date'] = Time.now.httpdate

    { :message=>'Hello, World!' }
  end

  # Test type 2: Single database query
  static_get '/db' do
    response['Date'] = Time.now.httpdate

    World.with_pk(rand1).values
  end

  # Test type 3: Multiple database queries
  static_get '/queries' do
    response['Date'] = Time.now.httpdate

    # Benchmark requirements explicitly forbid a WHERE..IN here, so be good
    randn(bounded_queries)
      .map! { |id| World.with_pk(id).values }
  end

  # Test type 4: Fortunes
  static_get '/fortunes' do
    response['Date'] = Time.now.httpdate

    @fortunes = Fortune.all
    @fortunes << Fortune.new(
      :id=>0,
      :message=>'Additional fortune added at request time.'
    )
    @fortunes.sort_by!(&:message)

    view :fortunes
  end

  # Test type 5: Database updates
  static_get '/updates' do
    response['Date'] = Time.now.httpdate

    # Benchmark requirements explicitly forbid a WHERE..IN here, transactions
    # are optional, batch updates are allowed (but each transaction can only
    # read and write a single record?), so... be good
    randn(bounded_queries).map! do |id|
      DB.transaction do
        world = World.for_update.with_pk(id)
        world.update(:randomnumber=>rand1)
        world.values
      end
    end
  end

  # Test type 6: Plaintext
  static_get '/plaintext' do
    response['Content-Type'] = 'text/plain'
    response['Date'] = Time.now.httpdate

    'Hello, World!'
  end

  # Even though we don't have any non-static routes, this is still required.
  route { |_| }
end
