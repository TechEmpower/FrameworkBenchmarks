module Models

type JsonMessage = { message : string }

[<Struct>]
type JsonStructMessage = { message : string }

 [<CLIMutable>]
type Fortune = { Id: int; Message: string }

[<Literal>]
let ConnectionString = "Server=127.0.0.1;Port=5432;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;"

type Implementation = Stock | Custom