module App.App

open State
open Router
open ExecNodes
open Middleware
open System.Threading.Tasks
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Builder



[<CLIMutable>][<Struct>] 
type JsonStructMessage = { message : string }


[<EntryPoint>]
let main _ = 
    
    let fallback : Zapp<_> = (fun ctx -> ctx {
        text "Url Not Found"
        status 404        
    })

    let webapp = 
        router [
            get "/plaintext" => (fun ctx -> ctx { text "Hello, World!" } )
            get "/json"      => (fun ctx -> ctx { json {JsonStructMessage.message = "Hello, World!"} } )
        ]

    WebHostBuilder()
        .UseKestrel()
        .Configure(fun app -> app.UseZebraMiddleware<string>("zebra",fallback,webapp) |> ignore)
        .Build()
        .Run()
    0