module App.App

open Microsoft.AspNetCore.Hosting
open Giraffe

[<CLIMutable>] 
type JsonMessage = { message : string }

[<CLIMutable>][<Struct>] 
type JsonStructMessage = { message : string }

type Implementation = Stock | Custom

module Routes =

    let stock : HttpHandler list = 
        [ route "/plaintext" >=> text "Hello, World!"
          route "/json" >=> json { JsonMessage.message = "Hello, World!" } ]

    let custom : HttpHandler list = 
        let inline contentLength (x:int32) = new System.Nullable<int64>( int64 x )

        let json data : HttpHandler =
            let bytes = Utf8Json.JsonSerializer.Serialize(data)
            fun _ ctx ->
                ctx.Response.ContentLength <- contentLength ( bytes.Length )
                ctx.Response.ContentType <- "application/json"
                ctx.Response.StatusCode <- 200
                ctx.WriteBytesAsync bytes

        let bytes = System.Text.Encoding.UTF8.GetBytes "Hello, World!"
        let text : HttpHandler = 
            fun _ ctx ->
                ctx.Response.ContentLength <- contentLength ( bytes.Length )
                ctx.Response.ContentType <- "text/plain"
                ctx.Response.StatusCode <- 200
                ctx.WriteBytesAsync bytes
    
        [ route "/plaintext" >=> text
          route "/json" >=> json { JsonStructMessage.message = "Hello, World!" } ]

let webApp implementation = 
    match implementation with
    | Stock -> GET >=> choose Routes.stock
    | Custom -> GET >=> choose Routes.custom

[<EntryPoint>]
let main args = 
    let implementation = 
        match args with
        | [| "stock" |]  -> Stock
        | _ -> Custom

    printfn "Running with %A implementation" implementation

    WebHostBuilder()
        .UseKestrel()
        .Configure(fun app -> app.UseGiraffe (webApp implementation))
        .ConfigureServices(fun services -> services.AddGiraffe() |> ignore)
        .Build()
        .Run()
    0