# frozen_string_literal: true

# Our Rack application to be executed by rackup
class HelloWorld < Roda
  plugin :hooks
  plugin :render, escape: true, layout_opts: { cache_key: "default_layout" }

  def bounded_queries
    queries = request.params["queries"].to_i
    queries.clamp(QUERIES_MIN, QUERIES_MAX)
  end

  # Return a random number between 1 and MAX_PK
  def rand1
    rand(MAX_PK) + 1
  end

  route do |r|
    response[DATE_HEADER] = Time.now.httpdate
    response[SERVER_HEADER] = SERVER_STRING if SERVER_STRING

    # Test type 1: JSON serialization
    r.is "json" do
      response[CONTENT_TYPE] = JSON_TYPE
      { message: "Hello, World!" }.to_json
    end

    # Test type 2: Single database query
    r.is "db" do
      response[CONTENT_TYPE] = JSON_TYPE
      World.with_pk(rand1).values.to_json
    end

    # Test type 3: Multiple database queries
    r.is "queries" do
      response[CONTENT_TYPE] = JSON_TYPE
      worlds =
        DB.synchronize do
          ALL_IDS.sample(bounded_queries).map do |id|
            World.with_pk(id).values
          end
        end
      worlds.to_json
    end

    # Test type 4: Fortunes
    r.is "fortunes" do
      response[CONTENT_TYPE] = HTML_TYPE
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
      response[CONTENT_TYPE] = JSON_TYPE
      worlds =
        DB.synchronize do
          ALL_IDS.sample(bounded_queries).map do |id|
            world = World.with_pk(id)
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
      response[CONTENT_TYPE] = PLAINTEXT_TYPE
      "Hello, World!"
    end
  end
end
