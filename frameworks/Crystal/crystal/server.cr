require "http/server"
require "json"
require "pg"
require "ecr"

APPDB = DB.open("postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=256&max_pool_size=256&max_idle_pool_size=256")
ID_MAXIMUM = 10_000

server = HTTP::Server.new do |context|
  response = context.response
  request = context.request

  response.headers["Server"] = "Crystal"
  response.headers["Date"] = HTTP.format_time(Time.now)
  
  case request.path
  when "/json"
    response.status_code = 200
    response.headers["Content-Type"] = "application/json"
    {message: "Hello, World!"}.to_json(response)
  when "/plaintext"
    response.status_code = 200
    response.headers["Content-Type"] = "text/plain"
    response.print "Hello, World!"
  when "/db"
    response.status_code = 200
    response.headers["Content-Type"] = "application/json"
    random_world.to_json(response)
  when "/queries"
    response.status_code = 200
    response.headers["Content-Type"] = "application/json"

    JSON.build(response) do |json|
      json.array do
        sanitized_query_count(request).times do
          random_world.to_json(json)
        end
      end
    end
  when "/fortunes"
    response.status_code = 200
    response.headers["Content-Type"] = "text/html; charset=UTF-8"

    data = fortunes
    additional_fortune = {
      id:      0,
      message: "Additional fortune added at request time.",
    }

    data.push(additional_fortune)
    data.sort! { |f1, f2| f1[:message] <=> f2[:message] }

    ECR.embed "views/fortunes.ecr", response
  when "/updates"
    response.status_code = 200
    response.headers["Content-Type"] = "application/json"

    JSON.build(response) do |json|
      json.array do
        sanitized_query_count(request).times do
          world = set_world({id: random_world[:id], randomNumber: rand(1..ID_MAXIMUM)})          
          world.to_json(json)
        end
      end
    end
  else
    response.status_code = 404
  end
end

private def random_world
  id = rand(1..ID_MAXIMUM)
  random_number = APPDB.query_one("SELECT randomNumber FROM world WHERE id = $1", id, as: Int32)
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
  queries = request.query_params["queries"].as(String)
  queries = queries.to_i? || 1
  queries.clamp(1..500)
end

server.listen("0.0.0.0", 8080, reuse_port: true)
