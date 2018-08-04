module App.App

open State
open Router
open ExecNodes
open Middleware

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore
open System




[<CLIMutable>][<Struct>] 
type JsonStructMessage = { message : string }

module Simple =
    let textFn<'T> (text:string) = 
        let bytes =         System.Text.Encoding.UTF8.GetBytes(text)
        let contentLength = Microsoft.Extensions.Primitives.StringValues(bytes.Length.ToString())
        let contentType =   Microsoft.Extensions.Primitives.StringValues "text/plain"
        fun (x:State<'T>) -> 
            x.HttpContext.Response.Headers.["Content-Length"] <- contentLength
            x.HttpContext.Response.Headers.["Content-Type"] <- contentType
            let t = x.HttpContext.Response.Body.WriteAsync(bytes,0,bytes.Length)
            let awt = t.GetAwaiter()
            x.CurrentState <- MachineState.Complete
            awt.OnCompleted x.Continue

    let inline jsonFn<'T> (value: ^a) =
        let bytes =         Utf8Json.JsonSerializer.Serialize< ^a>(value)
        let contentLength = Microsoft.Extensions.Primitives.StringValues(bytes.Length.ToString())
        let contentType =   Microsoft.Extensions.Primitives.StringValues "application/json"
        fun (x:State<'T>) -> 
            x.HttpContext.Response.Headers.["Content-Length"] <- contentLength
            x.HttpContext.Response.Headers.["Content-Type"] <- contentType
            let t = x.HttpContext.Response.Body.WriteAsync(bytes,0,bytes.Length)
            let awt = t.GetAwaiter()
            x.CurrentState <- MachineState.Complete
            awt.OnCompleted x.Continue

[<EntryPoint>]
let main args = 

    // Defualt implimentation        
    let fallback : Zapp<_> = (fun ctx -> ctx {
        text "Url Not Found"
        status 404        
    })

    let webapp = 
        router [
            get "/plaintext" => fun ctx -> ctx { text "Hello, World!" }
            get "/json"      => fun ctx -> ctx { json {JsonStructMessage.message = "Hello, World!"} }
        ]

    // Simple implimentation
    let plaintextPrint = Simple.textFn "Hello World!"
    let jsonPrint = Simple.jsonFn<unit> {JsonStructMessage.message = "Hello, World!"}
    let notFound = Simple.textFn "Not Found"

    let inline simpleApp (ctx:State<unit>) =
        match ctx.HttpContext.Request.Path.Value with
        | "/plaintext" -> plaintextPrint ctx
        | "/json"      -> jsonPrint      ctx
        | _            -> notFound       ctx


    // Config to used based on console arg
    let config : Action<Builder.IApplicationBuilder> =
            match args with
            | [|"simple"|] -> Action<Builder.IApplicationBuilder>( fun app -> app.UseZebraSimpleMiddleware(simpleApp) |> ignore )
            | _            -> Action<Builder.IApplicationBuilder>( fun app -> app.UseZebraMiddleware<unit>((),fallback,webapp) |> ignore )
        
    WebHostBuilder()
        .UseKestrel()
        .Configure(config)
        .Build()
        .Run()
    0