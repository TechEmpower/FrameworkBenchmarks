require "toro"
require "pg"

APPDB      = DB.open(ENV["DATABASE_URL"])
ID_MAXIMUM = 10_000

# Includes some monkey patches for Toro to make the benchmarks work
module Toro
  abstract class Router
    # By fefault html macro doesn't adds '; charset=UTF-8'
    macro html(template)
      header "Content-Type", "text/html; charset=UTF-8"
      render {{template}}
    end

    # Original write uses context.response.puts(str)
    # this adds 1 extra byte to the response \n that might affect the performance
    def write(str)
      context.response.print(str)
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

class App < Toro::Router
  def routes
    #
    # Basic Tests
    #

    # Test 1: JSON Serialization
    on "json" do
      get do
        header("Server", "Toro")
        header("Date", HTTP.format_time(Time.utc))
        json({message: "Hello, World!"})
      end
    end

    # Test 6: Plaintext
    on "plaintext" do
      get do
        header("Server", "Toro")
        header("Date", HTTP.format_time(Time.utc))
        text "Hello, World!"
      end
    end

    #
    # Postgres DatabaseTests
    #

    # Postgres Test 2: Single database query
    on "db" do
      get do
        header("Server", "Toro")
        header("Date", HTTP.format_time(Time.utc))
        json random_world
      end
    end

    on "queries" do
      get do
        results = (1..sanitized_query_count(context.request)).map do
          random_world
        end

        header("Server", "Toro")
        header("Date", HTTP.format_time(Time.utc))
        json results
      end
    end

    on "fortunes" do
      get do
        data = fortunes
        additional_fortune = {
          id:      0,
          message: "Additional fortune added at request time.",
        }
        data.push(additional_fortune)

        data.sort_by! { |fortune| fortune[:message] }

        header("Server", "Toro")
        header("Date", HTTP.format_time(Time.utc))
        html "views/fortunes"
      end
    end

    on "updates" do
      get do
        updated = (1..sanitized_query_count(context.request)).map do
          set_world({id: random_world[:id], randomNumber: rand(1..ID_MAXIMUM)})
        end

        header("Server", "Toro")
        header("Date", HTTP.format_time(Time.utc))
        json updated
      end
    end
  end
end

# Start the app on port 8080.
App.run(8080) do |server|
  server.listen("0.0.0.0", 8080, reuse_port: true)
end
