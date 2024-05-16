namespace App

open System
open System.Threading.Tasks
open Oxpecker

[<AutoOpen>]
module Common =

    [<CLIMutable>]
    type Fortune =
        {
            id      : int
            message : string
        }

    [<Literal>]
    let ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000"

    let fortuneComparer a b =
        String.CompareOrdinal(a.message, b.message)

[<RequireQualifiedAccess>]
module HtmlViews =
    open Oxpecker.ViewEngine

    let private fortunesHead =
        head() {
            title() { raw "Fortunes" }
        }

    let private layout (content: HtmlElement) =
        html() {
            fortunesHead
            body() { content }
        }

    let private fortunesTableHeader =
        tr() {
            th() { raw "id" }
            th() { raw "message" }
        }

    let fortunes (fortunes: Fortune[]) =
        table() {
            fortunesTableHeader
            for f in fortunes do
                tr() {
                    td() { raw <| string f.id }
                    td() { f.message }
                }
        } |> layout

[<RequireQualifiedAccess>]
module HttpHandlers =
    open Dapper
    open Npgsql

    let private extra =
        {
            id      = 0
            message = "Additional fortune added at request time."
        }

    let private fortunes : EndpointHandler =
        fun ctx ->
            task {
                use conn = new NpgsqlConnection(ConnectionString)
                let! data = conn.QueryAsync<Fortune>("SELECT id, message FROM fortune")
                let augmentedData = [|
                    yield! data
                    extra
                |]
                augmentedData |> Array.sortInPlaceWith fortuneComparer
                let view = HtmlViews.fortunes augmentedData
                return! ctx.WriteHtmlView view
            }

    type World =
        {
            id: int
            randomNumber: int
        }

    let private singleRow : EndpointHandler =
        fun ctx ->
            task {
                use conn = new NpgsqlConnection(ConnectionString)
                let! result = conn.QueryFirstOrDefaultAsync<World>(
                    "SELECT id, randomnumber FROM world WHERE id = @Id",
                    {| Id = Random.Shared.Next(1, 10001) |})
                return! ctx.WriteJsonChunked result
            }

    let utf8Const (s: string): EndpointHandler =
        let result = s |> System.Text.Encoding.UTF8.GetBytes
        fun ctx ->
            ctx.SetContentType("text/plain")
            bytes result ctx

    let endpoints : Endpoint[] =
        [|
            route "/plaintext" <| utf8Const "Hello, World!"
            route "/json"<| jsonChunked {| message = "Hello, World!" |}
            route "/fortunes" fortunes
            route "/db" singleRow
        |]


module Main =
    open SpanJson
    open Microsoft.AspNetCore.Http
    open Microsoft.AspNetCore.Builder
    open Microsoft.AspNetCore.Hosting
    open Microsoft.Extensions.DependencyInjection
    open Microsoft.Extensions.Hosting
    open Microsoft.Extensions.Logging
    open System.Buffers

    type SpanJsonSerializer() =
        interface Serializers.IJsonSerializer with
            member this.Serialize(value, ctx, chunked) =
                ctx.Response.ContentType <- "application/json; charset=utf-8"
                if chunked then
                    if ctx.Request.Method <> HttpMethods.Head then
                        JsonSerializer.Generic.Utf8.SerializeAsync<_>(value, stream = ctx.Response.Body).AsTask()
                    else
                        Task.CompletedTask
                else
                    task {
                        let buffer = JsonSerializer.Generic.Utf8.SerializeToArrayPool<_>(value)
                        ctx.Response.Headers.ContentLength <- buffer.Count
                        if ctx.Request.Method <> HttpMethods.Head then
                            do! ctx.Response.Body.WriteAsync(buffer)
                            ArrayPool<byte>.Shared.Return(buffer.Array)
                        else
                            return ()
                    }
            member this.Deserialize _ =
                failwith "Not implemented"

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)

        builder.Services
            .AddRouting()
            .AddOxpecker()
            .AddSingleton<Serializers.IJsonSerializer>(SpanJsonSerializer())
        |> ignore

        builder.Logging.ClearProviders() |> ignore
        builder.WebHost.ConfigureKestrel(fun options -> options.AllowSynchronousIO <- true) |> ignore

        let app = builder.Build()

        app.UseRouting()
           .UseOxpecker HttpHandlers.endpoints |> ignore

        app.Run()

        0