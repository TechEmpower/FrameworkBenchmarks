require "onyx-http"
require "json"
require "./models/*"

BENCH_DB = DB.open(ENV["DATABASE_URL"])

class CONTENT
  ID_MAX = 10_000
  JSON  = "application/json"
  PLAIN = "text/plain"
end

def get_world
  id = Random.rand(CONTENT::ID_MAX).succ
  rs = BENCH_DB.query("SELECT * FROM world WHERE id = $1", id)
  world = World.from_rs(rs).first
  # { id: id, randomNumber: world.randomNumber }
end

def set_world(world)
  BENCH_DB.exec("UPDATE world SET randomNumber = $1 WHERE id = $2", world[:randomNumber], world[:id])
  world
end

def sanitized_query_count(env)
  queries = env.request.path_params["queries"].as(String)
  queries = queries.to_i? || 1
  queries.clamp(1..500)
end

router = Onyx::HTTP::Router.new do
  get "/json" do |env|
    env.response.headers["Server"] = "Raze"
    env.response.headers["Date"] = HTTP.format_time(Time.now)
    env.response.content_type = CONTENT::JSON
    env.response << { message: "Hello, World!" }.to_json
  end

  get "/db" do |env|
    env.response.headers["Server"] = "Raze"
    env.response.headers["Date"] = HTTP.format_time(Time.now)
    env.response.content_type = CONTENT::JSON
    world = get_world
    env.response << { id: world.id, randomNumber: world.randomNumber }.to_json
  end

  get "/queries" do |env|
    env.response.headers["Server"] = "Raze"
    env.response.headers["Date"] = HTTP.format_time(Time.now)
    env.response.content_type = CONTENT::JSON
    results = (1..sanitized_query_count(env)).map do
      world = get_world
      { id: world.id, randomNumber: world.randomNumber }
    end
    results.to_json
  end

  get "/updates" do |env|
    env.response.headers["Server"] = "Raze"
    env.response.headers["Date"] = HTTP.format_time(Time.now)
    env.response.content_type = CONTENT::JSON
    updated = (1..sanitized_query_count(env)).map do
      random_number = Random.rand(CONTENT::ID_MAX).succ

      world = get_world
      changeset = world.changeset

      changeset.update(randomNumber: random_number)
      BENCH_DB.exec(*world.update(changeset).build(true))
      { id: world.id, randomNumber: random_number }
    end
    updated.to_json
  end

  get "/plaintext" do |env|
    env.response.headers["Server"] = "Raze"
    env.response.headers["Date"] = HTTP.format_time(Time.now)
    env.response.content_type = CONTENT::JSON
    env.response << "Hello, World!"
  end
end # router
logger = Onyx::HTTP::Logger.new
request_id = Onyx::HTTP::RequestID.new
response_time = Onyx::HTTP::ResponseTime.new

server = Onyx::HTTP::Server.new(response_time, request_id, logger, router)
server.bind_tcp(8080)
server.listen
