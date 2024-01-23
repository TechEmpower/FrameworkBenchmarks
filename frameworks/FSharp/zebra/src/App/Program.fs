module App.App

open State
open Router
open ExecNodes
open Middleware

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore
open System
open TemplateViewEngine
open Npgsql
open Dapper
open System.Collections.Generic


[<CLIMutable>][<Struct>]
type JsonStructMessage = { message : string }

 [<CLIMutable>]
type Fortune = { id: int; message: string }

[<Literal>]
let ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000"


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

    let inline jsonFn<'T> =
        let contentType =   Microsoft.Extensions.Primitives.StringValues "application/json"
        fun (x:State<'T>) ->
            let bytes =         Utf8Json.JsonSerializer.Serialize< ^a>({ message = "Hello, World!" })
            let contentLength = Microsoft.Extensions.Primitives.StringValues(bytes.Length.ToString())
            x.HttpContext.Response.Headers.["Content-Length"] <- contentLength
            x.HttpContext.Response.Headers.["Content-Type"] <- contentType
            let t = x.HttpContext.Response.Body.WriteAsync(bytes,0,bytes.Length)
            let awt = t.GetAwaiter()
            x.CurrentState <- MachineState.Complete
            awt.OnCompleted x.Continue

module View =

    let fortuneView =
        html [] [
            head [] [
                title []  [ rawText "Fortunes" ]
            ]
            body [] [
                table [] [
                    tr [] [
                        th [] [ rawText "id" ]
                        th [] [ rawText "message" ]
                    ]
                    bindFor<_,_> (fun ls -> ls :> seq<Fortune> ) (
                        tr [] [
                            td [] [ bindInt (fun v -> v.id) ]
                            td [] [ bindStr (fun v -> v.message) ]
                        ]
                    )
                ]
            ]
        ] |> compileDoc

    let extra = { id = 0; message = "Additional fortune added at request time." }
    let FortuneComparer = { new IComparer<Fortune> with
        member self.Compare(a,b) = String.CompareOrdinal(a.message, b.message)
}

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
            get "/fortunes"  => fun ctx -> ctx {
                use conn = new NpgsqlConnection(ConnectionString)

                let! (data : Fortune seq) = conn.QueryAsync<Fortune>("SELECT id, message FROM fortune")

                let fortunes =
                    let xs = data.AsList()
                    xs.Add View.extra
                    xs.Sort View.FortuneComparer
                    xs

                ctx.Render( fortunes, View.fortuneView )
                }
        ]

    // Simple implimentation
    let plaintextPrint = Simple.textFn "Hello, World!"
    let jsonPrint = Simple.jsonFn<_>
    let notFound = Simple.textFn "Not Found"

    let inline simpleApp (ctx:State<_>) =
        match ctx.HttpContext.Request.Path.Value with
        | "/plaintext" -> plaintextPrint ctx
        | "/json"      -> jsonPrint      ctx
        | _            -> notFound       ctx

    // Config to used based on console arg
    let config : Action<Builder.IApplicationBuilder> =
            match args with
            | [|"simple"|] ->
                printfn "Using Simple Config..."
                Action<Builder.IApplicationBuilder>( fun app -> app.UseZebraSimpleMiddleware<int>(0,simpleApp) |> ignore )
            | _            ->
                printfn "Using Stock Config..."
                Action<Builder.IApplicationBuilder>( fun app -> app.UseZebraMiddleware<int>(0,fallback,webapp) |> ignore )

    WebHostBuilder()
        .UseKestrel()
        .Configure(config)
        .Build()
        .Run()
    0
