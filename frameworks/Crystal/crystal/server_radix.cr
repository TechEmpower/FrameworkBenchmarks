require "http/server"
require "json"
require "pg"
require "ecr"
require "radix"

APPDB = DB.open(ENV["DATABASE_URL"])
ID_MAXIMUM = 10_000
CONTENT_HTML = "text/html; charset=UTF-8"
CONTENT_JSON = "application/json"
CONTENT_TEXT = "text/plain"

plaintext_handler = ->(context : HTTP::Server::Context) do
  context.response.tap do |response|
    response.status_code = 200
    response.headers["Content-Type"] = CONTENT_TEXT
    response.print "Hello, World!"
  end
end

json_handler = ->(context : HTTP::Server::Context) do
  context.response.tap do |response|
    response.status_code = 200
    response.headers["Content-Type"] = CONTENT_JSON
    json = {message: "Hello, World!"}.to_json
    response.print(json)
  end
end

db_handler = ->(context : HTTP::Server::Context) do
  context.response.tap do |response|
    response.status_code = 200
    response.headers["Content-Type"] = CONTENT_JSON
    json = find_world(rand(1..ID_MAXIMUM)).to_json
    response.print(json)
  end
end

queries_handler = ->(context : HTTP::Server::Context) do
  request = context.request
  context.response.tap do |response|
    response.status_code = 200
    response.headers["Content-Type"] = CONTENT_JSON

    worlds = (1..sanitized_query_count(request)).map { find_world(rand(1..ID_MAXIMUM)) }
    response.print(worlds.to_json)
  end
end

fortunes_handler = ->(context : HTTP::Server::Context) do
  context.response.tap do |response|
    response.status_code = 200
    response.headers["Content-Type"] = CONTENT_HTML

    data = APPDB.query_all("SELECT id, message FROM Fortune", as: {id: Int32, message: String})
    additional_fortune = {
      id:      0,
      message: "Additional fortune added at request time.",
    }

    data.push(additional_fortune)
    data.sort_by! { |fortune| fortune[:message] }

    ECR.embed "views/fortunes.ecr", response
  end
end

updates_handler = ->(context : HTTP::Server::Context) do
  request = context.request
  context.response.tap do |response|
    response.status_code = 200
    response.headers["Content-Type"] = "application/json"

    worlds = (1..sanitized_query_count(request)).map do
      world = find_world(rand(1..ID_MAXIMUM))
      random_number = rand(1..ID_MAXIMUM)
      while random_number == world[:randomNumber]
        random_number = rand(1..ID_MAXIMUM)
      end
      APPDB.exec("UPDATE world SET randomNumber = $1 WHERE id = $2", random_number, world[:id])
      {id: world[:id], randomNumber: random_number}
    end
    response.print(worlds.to_json)
  end
end

tree = Radix::Tree(Proc(HTTP::Server::Context, HTTP::Server::Response)).new
tree.add "/plaintext", plaintext_handler
tree.add "/json", json_handler
tree.add "/db", db_handler
tree.add "/queries", queries_handler
tree.add "/fortunes", fortunes_handler
tree.add "/updates", updates_handler

server = HTTP::Server.new do |context|
  request = context.request
  response = context.response
  response.headers["Server"] = "Crystal"
  response.headers["Date"] = HTTP.format_time(Time.local)

  result = tree.find(request.path)

  if result.found?
    result.payload.call(context)
  else
    context.response.status_code = 404
  end
end

private def find_world(id : Int32)
  APPDB.query_one("SELECT id, randomNumber FROM world WHERE id = $1", id, as: {id: Int32, randomNumber: Int32})
end

private def sanitized_query_count(request)
  queries = request.query_params["queries"].as(String)
  queries = queries.to_i? || 1
  queries.clamp(1..500)
end

server.listen("0.0.0.0", 8080, reuse_port: true)
