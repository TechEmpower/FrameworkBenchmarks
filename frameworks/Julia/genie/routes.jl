using Genie
using Genie.Router
using Genie.Renderer
using Genie.Renderer.Json
using Genie.Responses
import SearchLight: AbstractModel, DbId
import Base: @kwdef
using Dates
using LibPQ

function date_now_header()
   "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT"
end

const contentTypePlainText = "Content-Type" => "text/plain"

route("/plaintext") do
   respond("Hello, World!", 200, Genie.Renderer.HTTPHeaders(contentTypePlainText, date_now_header()))
end

struct JsonMessage 
   message::String
end

route("/json") do
   json(JsonMessage("Hello, World!"), headers = Genie.Renderer.HTTPHeaders(date_now_header()))
end

@kwdef mutable struct World <: AbstractModel
  id::DbId = DbId()
  randomNumber::Int = 0
end

@kwdef mutable struct Fortune <: AbstractModel
  id::DbId = DbId()
  message::String = ""
end

route("/json") do
  randNumber = rand(1:10000)

  json(JsonMessage("Hello, World!"), headers = Genie.Renderer.HTTPHeaders(date_now_header()))
end

route("/db") do
  randNumber = rand(1:10000)
  
  conn = LibPQ.Connection("dbname=hello_world host=tfb-database user=benchmarkdbuser password=benchmarkdbpass")
  try 
    result = execute(conn, "SELECT id, randomNumber FROM world WHERE id = \$1", [randNumber])
    data = columntable(result)
    json(data, headers = Genie.Renderer.HTTPHeaders(date_now_header()))
  finally
    close(conn)
  end
  
end