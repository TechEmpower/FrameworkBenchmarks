require "grip"
require "pg"
require "ecr/macros"

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

class Json < Grip::Controllers::Http
  def get(context)
    context
      .put_resp_header("Server", "Grip")
      .put_resp_header("Date", HTTP.format_time(Time.utc))
      .json(
        {
          message: "Hello, World!",
        }
      )
  end
end

class Plaintext < Grip::Controllers::Http
  def get(context)
    context
      .put_resp_header("Server", "Grip")
      .put_resp_header("Date", HTTP.format_time(Time.utc))
      .text(
        "Hello, World!"
      )
  end
end

class Db < Grip::Controllers::Http
  def get(context)
    context
      .put_resp_header("Server", "Grip")
      .put_resp_header("Date", HTTP.format_time(Time.utc))
      .json(
        random_world
      )
  end
end

class Queries < Grip::Controllers::Http
  def get(context)
    results = (1..sanitized_query_count(context.request)).map do
      random_world
    end

    context
      .put_resp_header("Server", "Grip")
      .put_resp_header("Date", HTTP.format_time(Time.utc))
      .json(
        results
      )
  end
end

class Updates < Grip::Controllers::Http
  def get(context)
    updated = (1..sanitized_query_count(context.request)).map do
      set_world({id: random_world[:id], randomNumber: rand(1..ID_MAXIMUM)})
    end

    context
      .put_resp_header("Server", "Grip")
      .put_resp_header("Date", HTTP.format_time(Time.utc))
      .json(
        updated
      )
  end
end

class Fortunes < Grip::Controllers::Http
  def get(context)
    data = fortunes
    additional_fortune = {
      id:      0,
      message: "Additional fortune added at request time.",
    }
    data.push(additional_fortune)
    data.sort_by! { |fortune| fortune[:message] }

    io = IO::Memory.new
    ECR.embed "views/fortunes.ecr", io

    context
      .put_resp_header("Server", "Grip")
      .put_resp_header("Date", HTTP.format_time(Time.utc))
      .html(
        io.to_s
      )
  end
end

class Application < Grip::Application
  def routes
    get "/json", Json
    get "/plaintext", Plaintext
    get "/db", Db
    get "/queries", Queries
    get "/updates", Updates
    get "/fortunes", Fortunes
  end

  def router : Array(HTTP::Handler)
    [
      @http_handler,
    ] of HTTP::Handler
  end

  def server : HTTP::Server
    HTTP::Server.new(@router)
  end

  def reuse_port
    true
  end

  def host
    "0.0.0.0"
  end

  def port
    8080
  end
end

app = Application.new
app.run
