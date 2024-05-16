namespace App

open System
open System.Collections.Generic
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

    let FortuneComparer = {
        new IComparer<Fortune> with
            member self.Compare(a,b) = String.CompareOrdinal(a.message, b.message)
    }

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
    open System.Text
    open Microsoft.AspNetCore.Http

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
                Array.Sort(augmentedData, FortuneComparer)
                let view = HtmlViews.fortunes augmentedData
                return! ctx.WriteHtmlView view
            }

    [<Struct>]
    [<CLIMutable>]
    type World =
        {
            id: int
            randomnumber: int
        }

    let private readSingleRow (conn: NpgsqlConnection) =
        conn.QueryFirstOrDefaultAsync<World>(
            "SELECT id, randomnumber FROM world WHERE id = @Id",
            {| Id = Random.Shared.Next(1, 10001) |}
        )

    let private parseQueries (ctx: HttpContext) =
        match ctx.TryGetRouteValue<string>("count") with
        | Some q ->
            match Int32.TryParse q with
            | true, q when q > 1 -> if q < 500 then q else 500
            | _, _ -> 1
        | _ -> 1

    let private singleQuery : EndpointHandler =
        fun ctx ->
            task {
                use conn = new NpgsqlConnection(ConnectionString)
                let! result = readSingleRow conn
                return! ctx.WriteJsonChunked result
            }

    let private multipleQueries : EndpointHandler =
        fun ctx ->
            let count = parseQueries ctx
            let results = Array.zeroCreate<World> count
            task {
                use conn = new NpgsqlConnection(ConnectionString)
                do! conn.OpenAsync()
                for i in 0..results.Length-1 do
                    let! result = readSingleRow conn
                    results[i] <- result
                return! ctx.WriteJsonChunked results
            }

    let private maxBatch = 500
    let mutable private queries = Array.zeroCreate (maxBatch + 1)

    let private batchUpdateString batchSize =
        match queries[batchSize] with
        | null ->
            let lastIndex = batchSize - 1
            let sb = StringBuilder()
            sb.Append("UPDATE world SET randomNumber = temp.randomNumber FROM (VALUES ") |> ignore
            for i in 0..lastIndex-1 do
                sb.AppendFormat("(@Id_{0}, @Rn_{0}), ", i) |> ignore
            sb.AppendFormat("(@Id_{0}, @Rn_{0}) ORDER BY 1) AS temp(id, randomNumber) WHERE temp.id = world.id", lastIndex) |> ignore
            let result = sb.ToString()
            queries[batchSize] <- result
            result
        | q -> q

    let private multipleUpdates : EndpointHandler =
        fun ctx ->
            let count = parseQueries ctx
            let results = Array.zeroCreate<World> count
            task {
                use conn = new NpgsqlConnection(ConnectionString)
                do! conn.OpenAsync()
                for i in 0..results.Length-1 do
                    let! result = readSingleRow conn
                    results[i] <- result
                let parameters = Dictionary<string,obj>()
                for i in 0..results.Length-1 do
                    let randomNumber = Random.Shared.Next(1, 10001)
                    parameters[$"@Rn_{i}"] <- randomNumber
                    parameters[$"@Id_{i}"] <- results[i].id
                    results[i] <- { results[i] with randomnumber = randomNumber }
                let! _ = conn.ExecuteAsync(batchUpdateString count, parameters)
                return! ctx.WriteJsonChunked results
            }

    let utf8Const (s: string): EndpointHandler =
        let result = s |> Encoding.UTF8.GetBytes
        fun ctx ->
            ctx.SetContentType("text/plain")
            bytes result ctx

    let endpoints =
        [|
            route "/plaintext" <| utf8Const "Hello, World!"
            route "/json"<| jsonChunked {| message = "Hello, World!" |}
            route "/fortunes" fortunes
            route "/db" singleQuery
            route "/queries/{count?}" multipleQueries
            route "/updates/{count?}" multipleUpdates
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
                ctx.Response.ContentType <- "application/json"
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

        //builder.Logging.ClearProviders() |> ignore
        builder.WebHost.ConfigureKestrel(fun options -> options.AllowSynchronousIO <- true) |> ignore

        let app = builder.Build()

        app.UseRouting()
           .UseOxpecker HttpHandlers.endpoints |> ignore

        app.Run()

        0