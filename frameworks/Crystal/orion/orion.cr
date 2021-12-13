require "orion"
require "json"
require "ecr/macros"
require "pg"

APPDB      = DB.open(ENV["DATABASE_URL"])
ID_MAXIMUM = 10_000

private def random_world
  id = rand(1..ID_MAXIMUM)
  id, random_number = APPDB.query_one("SELECT id, randomNumber FROM world WHERE id = $1", id, as: {Int32, Int32})
  {id: id, randomNumber: random_number}
end

private def set_world(world)
  APPDB.exec("UPDATE world SET randomNumber = $1 WHERE id = $2", world[:randomNumber], world[:id])
  world
end

private def fortunes
  data = Array(NamedTuple(id: Int32, message: String)).new

  APPDB.query_each("SELECT id, message FROM Fortune") do |rs|
    data.push({id: rs.read(Int32), message: rs.read(String)})
  end

  data
end

private def sanitized_query_count(request)
  queries = request.query_params["queries"]? || "1"
  queries = queries.to_i? || 1
  queries.clamp(1..500)
end

router Bench do
  #
  # Basic Tests
  #

  # Test 1: JSON Serialization
  get "/json" do |context|
    context.response.headers["Server"] = "Orion"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    context.response.headers["content-type"] = "application/json"

    context.response.puts({message: "Hello, World!"}.to_json)
  end

  # Test 2: Plaintext
  get "/plaintext" do |context|
    context.response.headers["Server"] = "Orion"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    context.response.headers["content-type"] = "text/plain"

    context.response.print "Hello, World!"
  end

  #
  # Postgres DatabaseTests
  #

  # Postgres Test 3: Single database query
  get "/db" do |context|
    context.response.headers["Server"] = "Orion"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    context.response.headers["content-type"] = "application/json"

    context.response.puts random_world.to_json
  end

  # Postgres Test 4: Multiple database query
  get "/queries" do |context|
    results = (1..sanitized_query_count(context.request)).map do
      random_world
    end
    context.response.headers["Server"] = "Orion"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    context.response.headers["content-type"] = "application/json"

    context.response.puts results.to_json
  end

  # Postgres Test 5: HMTL template render
  get "/fortunes" do |context|
    data = fortunes
    additional_fortune = {
      id:      0,
      message: "Additional fortune added at request time.",
    }
    data.push(additional_fortune)
    data.sort_by! { |fortune| fortune[:message] }
    io = IO::Memory.new
    ECR.embed "views/fortunes.ecr", io

    context.response.headers["Server"] = "Orion"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    context.response.headers["content-type"] = "text/html; charset=UTF-8"

    context.response.puts io.to_s
  end

  # Postgres Test 6: Data updates
  get "/updates" do |context|
    updated = (1..sanitized_query_count(context.request)).map do
      set_world({id: random_world[:id], randomNumber: rand(1..ID_MAXIMUM)})
    end
    context.response.headers["Server"] = "Orion"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    context.response.headers["content-type"] = "application/json"

    context.response.puts updated.to_json
  end
end

Bench.listen(host: "0.0.0.0", port: 8080, reuse_port: true)