module App.App

open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Giraffe

[<CLIMutable>]
type JsonMessage = { message : string }

let jsonutf8 (data:obj) : HttpHandler =
    fun (_ : HttpFunc) (ctx : HttpContext) ->
        let bytes = Utf8Json.JsonSerializer.Serialize(data)
        ctx.SetContentType "application/json"
        ctx.Response.ContentLength <- new System.Nullable<int64>( int64 bytes.Length )
        ctx.WriteBytesAsync bytes

let webApp =
    choose [
        GET >=>
            choose [
                route "/plaintext" >=> text "Hello, World!"
                route "/json" >=> json { message = "Hello, World!" }
                route "/jsonutf8" >=> jsonutf8 { message = "Hello, World!" }
            ]
        setStatusCode 404 >=> text "Not Found" 
    ]
            
[<EntryPoint>]
let main _ =    
    WebHostBuilder()
        .UseKestrel()        
        .Configure(fun app -> app.UseGiraffe(webApp))
        .ConfigureServices(fun services -> services.AddGiraffe() |> ignore)
        .Build()
        .Run()
    0