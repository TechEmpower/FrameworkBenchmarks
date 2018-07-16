module App.App

open State
open Router
open ExecNodes
open System.Threading.Tasks
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Builder
open Giraffe

type ZebraMiddleware<'T>(
                        next          : RequestDelegate,
                        Dependencies  : 'T,
                        failAction    : State<'T> -> unit,
                        AppBuilder    : PipeLine<'T>
                        ) =
    let finishNode = FinishNode() :> INode<'T>
    let failNode = FailNode<'T>(failAction,finishNode)
    let appNode = AppBuilder(finishNode,failNode) // build App node

    do System.GC.Collect() // AppBuilder creates alot of garbage
    
    member __.Invoke (ctx : HttpContext) = 
        
        let amb = AsyncTaskMethodBuilder()
        let state = State<'T>(ctx,Dependencies,amb)
        appNode.Apply state
        amb.Task


type IApplicationBuilder with
    member x.UseZebraMiddleware<'T>(dependencies:'T,fallback:Zapp<'T>,app:PipeLine<'T>) = 
        x.UseMiddleware<ZebraMiddleware<'T>> [|box dependencies;box fallback;box app|] 

// [<CLIMutable>] 
// type JsonMessage = { message : string }

[<CLIMutable>][<Struct>] 
type JsonStructMessage = { message : string }


[<EntryPoint>]
let main _ = 
    
    let fallback : Zapp<_> = (fun ctx -> ctx {
        ctx.Text "Url Not Found"
        ctx.SetStatus 404        
    })

    let webapp = 
        router [
            get "/plaintext" => (fun ctx -> ctx { ctx.Text "Hello, World!" } )
            get "/json"      => (fun ctx -> ctx { ctx.Json {JsonStructMessage.message = "Hello, World!"} } )
        ]

    WebHostBuilder()
        .UseKestrel()
        .Configure(fun app -> app.UseZebraMiddleware<string>("zebra",fallback,webapp) |> ignore)
        .Build()
        .Run()
    0