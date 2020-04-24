require "grip"
require "pg"
require "ecr/macros"

APPDB      = DB.open(ENV["DATABASE_URL"])
ID_MAXIMUM = 10_000

module Grip
  module DSL
    module Methods
       def html(context, content, status_code = HTTP::Status::OK)
        context.response.status_code = status_code.to_i
        context.response.headers.merge!({"Content-Type" => "text/html; charset=UTF-8"})
        context.response.print(content)
        context.response
      end
    end
  end
end

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

class Json < Grip::Controller::Http
  def get(context)
    context.response.headers["Server"] = "Grip"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    json(
      context,
      {
        message: "Hello, World!",
      }
    )
  end
end

class Plaintext < Grip::Controller::Http
  def get(context)
    context.response.headers["Server"] = "Grip"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    text(
      context,
      "Hello, World!"
    )
  end
end

class Db < Grip::Controller::Http
  def get(context)
    context.response.headers["Server"] = "Grip"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    json(
      context,
      random_world
    )
  end
end

class Queries < Grip::Controller::Http
  def get(context)
    results = (1..sanitized_query_count(context.request)).map do
      random_world
    end

    context.response.headers["Server"] = "Grip"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    json(
      context,
      results
    )
  end
end

class Updates < Grip::Controller::Http
  def get(context)
    updated = (1..sanitized_query_count(context.request)).map do
      set_world({id: random_world[:id], randomNumber: rand(1..ID_MAXIMUM)})
    end
    context.response.headers["Server"] = "Grip"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)
    json(
      context,
      updated
    )
  end
end

class Fortunes < Grip::Controller::Http
  def get(context)
    data = fortunes
    additional_fortune = {
      id:      0,
      message: "Additional fortune added at request time.",
    }
    data.push(additional_fortune)
    data.sort_by! { |fortune| fortune[:message] }

    context.response.headers["Server"] = "Grip"
    context.response.headers["Date"] = HTTP.format_time(Time.utc)

    io = IO::Memory.new
    ECR.embed "views/fortunes.ecr", io
    html(
      context,
      io.to_s
    )
  end
end

class Application < Grip::Application
  def initialize
    get "/json", Json
    get "/plaintext", Plaintext
    get "/db", Db
    get "/queries", Queries
    get "/updates", Updates
    get "/fortunes", Fortunes
  end
end

app = Application.new
app.run(8080) do |config|
  server = config.server.not_nil!
  server.bind_tcp "0.0.0.0", 8080, reuse_port: true
end
