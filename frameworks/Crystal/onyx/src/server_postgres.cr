require "pg"
require "json"
require "onyx/rest"
require "onyx-sql"


BENCH_DB = DB.open(ENV["DATABASE_URL"])

class CONTENT
  JSON  = "application/json"
  PLAIN = "text/plain"
end

# Models
class World
  include Onyx::SQL::Model

  schema world do
    pkey id : Int32
    type randomnumber : Int32
  end
end

class Fortune
  include Onyx::SQL::Model

  schema fortune do
    pkey id : Int32
    type message : String
  end
end

# Helper methods
def random_id
  Random.rand(10_000).succ
end

def get_world
  rs = BENCH_DB.query("SELECT * FROM world WHERE id = $1", random_id)
  world = World.from_rs(rs).first
end

struct QueriesParams
  include HTTP::Params::Serializable
  getter queries : Int32
end

def sanitized_query_count(env)
  query = env.request.query
  queries = (query ? QueriesParams.from_query(query).queries.to_i : 1) rescue 1
  queries.clamp(1..500)
end

Onyx.draw do
  get "/json" do |env|
    env.response << { message: "Hello, World!" }.to_json
  end

  get "/db" do |env|
    world = get_world
    env.response << { id: world.id, randomnumber: world.randomnumber }.to_json
  end

  get "/queries" do |env|
    results = (1..sanitized_query_count(env)).map do
      world = get_world
      { id: world.id, randomnumber: world.randomnumber }
    end
    env.response << results.to_json
  end

  get "/updates" do |env|
    updated = (1..sanitized_query_count(env)).map do
      random_number = random_id
      world = get_world
      changeset = world.changeset
      changeset.update(randomnumber: random_number)

      BENCH_DB.exec(*world.update(changeset).build(true))
      { id: world.id, randomnumber: random_number }
    end
    env.response << updated.to_json
  end

  get "/plaintext" do |env|
    env.response.content_type = CONTENT::PLAIN
    env.response << "Hello, World!"
  end
end

class CustomHandler
  include HTTP::Handler

  def call(context)
    context.response.headers["Server"] = "Onyx"
    context.response.headers["Date"] = HTTP.format_time(Time.now)
    context.response.content_type = CONTENT::JSON
    call_next(context)
  end
end

Onyx.listen(ENV["TEST_HOST"], 8080) do
  handlers.insert(2, CustomHandler.new)
end


