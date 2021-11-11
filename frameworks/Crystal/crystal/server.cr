require "http/server"
require "json"
require "pg"
require "ecr"

APPDB = DB.open(ENV["DATABASE_URL"])
ID_MAXIMUM = 10_000
CONTENT_HTML = "text/html; charset=UTF-8"
CONTENT_JSON = "application/json"
CONTENT_TEXT = "text/plain"

server = HTTP::Server.new do |context|
  response = context.response
  request = context.request

  response.headers["Server"] = "Crystal"
  response.headers["Date"] = HTTP.format_time(Time.local)

  case request.path
  when "/json"
    response.status_code = 200
    response.headers["Content-Type"] = CONTENT_JSON
    json = {message: "Hello, World!"}.to_json
    response.print(json)
  when "/plaintext"
    response.status_code = 200
    response.headers["Content-Type"] = CONTENT_TEXT
    response.print "Hello, World!"
  when "/db"
    response.status_code = 200
    response.headers["Content-Type"] = CONTENT_JSON
    json = find_world(rand(1..ID_MAXIMUM)).to_json
    response.print(json)
  when "/queries"
    response.status_code = 200
    response.headers["Content-Type"] = CONTENT_JSON

    worlds = (1..sanitized_query_count(request)).map { find_world(rand(1..ID_MAXIMUM)) }
    response.print(worlds.to_json)
  when "/fortunes"
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
  when "/updates"
    response.status_code = 200
    response.headers["Content-Type"] = CONTENT_JSON
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
  else
    response.status_code = 404
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
