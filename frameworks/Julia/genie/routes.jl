using Genie, Genie.Router, Genie.Renderer, Genie.Renderer.Json, Genie.Responses
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

function with_connection(operation)
  connection = DBInterface.connect(MySQL.Connection, "tfb-database", "benchmarkdbuser", "benchmarkdbpass", db="hello_world")
  try
    return operation(connection)
  finally
    DBInterface.close!(connection)
  end
end

function query_world(connection, id::Int)::World
  sqlQuery = "SELECT id, randomNumber FROM World WHERE id = $id"
  results = DBInterface.execute(connection, sqlQuery)
  row = first(results)
  return World(row[1], row[2])
end

route("/db") do
  with_connection(function (connection)
  randomnumber = rand(1:10000)
    world = query_world(connection, randomnumber)
    json(world, headers=Genie.Renderer.HTTPHeaders(date_now_header()))
  end)
end

function numbers_of_queries(paramname::Symbol; default="")
  number_of_queries_param = params(paramname, default)

  number_of_queries = 1  
  try
    number_of_queries = parse(Int64, number_of_queries_param)
  catch _
    number_of_queries = 1
  end

  if number_of_queries > 500
    number_of_queries = 500
  end

  if number_of_queries < 1
    number_of_queries = 1
  end

  return number_of_queries
end

route("/queries") do  
  number_of_queries = numbers_of_queries(:queries)
  
  worlds = World[]
  with_connection(function (connection)
    for _ in 1:number_of_queries
      randomnumber = rand(1:10000)
      world = query_world(connection, randomnumber)
      push!(worlds, world)
    end
  end)

  json(worlds, headers=Genie.Renderer.HTTPHeaders(date_now_header()))
end

route("/updates") do  
  number_of_queries = numbers_of_queries(:queries)
  
  worlds = World[]
  with_connection(function (connection)
    for _ in 1:number_of_queries
      randomnumber = rand(1:10000)
      world = query_world(connection, randomnumber)
      push!(worlds, world)
    end

    worlds = map(world -> World(world.id, rand(1:10000)), worlds)
    
    ids = Int[]
    random_numbers = Int[]
    [(push!(ids, world.id); push!(random_numbers, world.randomNumber)) for world in worlds]

    DBInterface.transaction(function ()
      update_statement = DBInterface.prepare(connection, "UPDATE world SET randomNumber=? WHERE id=?") 
      try
        DBInterface.executemany(update_statement, (randomNumber=random_numbers, id=ids))
      finally
        DBInterface.close!(update_statement)
      end
    end, connection)
  end)

  json(worlds, headers=Genie.Renderer.HTTPHeaders(date_now_header()))
end