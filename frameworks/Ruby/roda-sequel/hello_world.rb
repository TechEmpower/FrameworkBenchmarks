# frozen_string_literal: true

# Our Rack application to be executed by rackup
class HelloWorld < Roda
  plugin :hooks
  plugin :render, escape: true, layout_opts: { cache_key: "default_layout" }

  def bounded_queries
    queries = request.params["queries"].to_i
    return QUERIES_MIN if queries < QUERIES_MIN
    return QUERIES_MAX if queries > QUERIES_MAX
    queries
  end

  # Return a random number between 1 and MAX_PK
  def rand1
    rand(MAX_PK) + 1
  end

  route do |r|
    response["Date"] = Time.now.httpdate
    response["Server"] = SERVER_STRING if SERVER_STRING
    #default content type
    response["Content-Type"] = "application/json"

    # Test type 1: JSON serialization
    r.is "json" do
      { message: "Hello, World!" }.to_json
    end

    # Test type 2: Single database query
    r.is "db" do
      World.with_pk(rand1).values.to_json
    end

    # Test type 3: Multiple database queries
    r.is "queries" do
      worlds =
        DB.synchronize do
          Array.new(bounded_queries) { World.with_pk(rand1).values }
        end
      worlds.to_json
    end

    # Test type 4: Fortunes
    r.is "fortunes" do
      response["Content-Type"] = "text/html; charset=utf-8"
      @fortunes = Fortune.all
      @fortunes << Fortune.new(
        id: 0,
        message: "Additional fortune added at request time."
      )
      @fortunes.sort_by!(&:message)
      view :fortunes
    end

    # Test type 5: Database updates
    r.is "updates" do
      worlds =
        DB.synchronize do
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
    r.is "plaintext" do
      response["Content-Type"] = "text/plain"
      "Hello, World!"
    end
  end
end
