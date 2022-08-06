using Genie, Genie.Router
using Genie.Renderer
using Genie.Renderer.Json
using Genie.Responses
using Dates
using MySQL, DBInterface


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
  json(JsonMessage("Hello, World!"), headers=Genie.Renderer.HTTPHeaders(date_now_header()))
end

route("/json") do
  randNumber = rand(1:10000)

  json(JsonMessage("Hello, World!"), headers=Genie.Renderer.HTTPHeaders(date_now_header()))
end

struct World
  id::Int
  randomNumber::Int
end

function withConnection(operation)
  env = ENV["GENIE_ENV"]


  connection = if (env == "prod")
    DBInterface.connect(MySQL.Connection, "tfb-database", "benchmarkdbuser", "benchmarkdbpass", db="hello_world")
  else
    DBInterface.connect(MySQL.Connection, "127.0.0.1", "root", "benchmarkdbpass", db="hello_world")
  end
  try
    return operation(connection)
  finally
    DBInterface.close!(connection)
  end
end

function queryWorld(connection, id::Int)::World
  sqlQuery = "SELECT id, randomNumber FROM World WHERE id = $id"
  results = DBInterface.execute(connection, sqlQuery)
  row = first(results)
  return World(row[1], row[2])
end

route("/db") do
  withConnection(function (connection)
    randomNumber = rand(1:10000)
    world = queryWorld(connection, randomNumber)
    json(world, headers=Genie.Renderer.HTTPHeaders(date_now_header()))
  end)
end

route("/queries") do  
  numQueriesParam = params(:queries, "")

  numQueries = 1  
  try
    numQueries = parse(Int64, numQueriesParam)
  catch ArgumentError
    numQueries = 1
  end

  if numQueries > 500
    numQueries = 500
  end

  if numQueries < 1
    numQueries = 1
  end
  
  worlds = World[]
  withConnection(function (connection)
    for _ in 1:numQueries
      randomNumber = rand(1:10000)
      world = queryWorld(connection, randomNumber)
      push!(worlds, world)
    end
  end)

  json(worlds, headers=Genie.Renderer.HTTPHeaders(date_now_header()))
end