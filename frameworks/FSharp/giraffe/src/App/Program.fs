namespace App

[<AutoOpen>]
module Common =
    open System
    open System.Collections.Generic

    [<CLIMutable>]
    type Fortune = { id: int; message: string }

    [<CLIMutable>]
    type World = { id: int; randomNumber: int }

    [<Literal>]
    let ConnectionString =
        """
        Server=tfb-database;
        Database=hello_world;
        User Id=benchmarkdbuser;
        Password=benchmarkdbpass;
        SSL Mode=Disable;
        Maximum Pool Size=1024;
        NoResetOnClose=true;
        Enlist=false;
        Max Auto Prepare=4;
        Multiplexing=true;
        Write Coalescing Buffer Threshold Bytes=1000
        """

    [<Struct>]
    type JsonMode =
        | System
        | Newtonsoft
        | FSharpFriendly

    let FortuneComparer =
        { new IComparer<Fortune> with
            member __.Compare (a, b) =
                String.CompareOrdinal (a.message, b.message)
        }

    type RandomUtil() =
        let rnd = System.Random ()
        let min = 1
        let max = 10_000

        member _.Next () = rnd.Next (min, max)

    [<Literal>]
    let MaxDegreeOfParallelism = 3

[<RequireQualifiedAccess>]
module HtmlViews =
    open Giraffe.ViewEngine

    let private fortunesHead = head [] [ title [] [ rawText "Fortunes" ] ]

    let private layout (content: XmlNode list) =
        html [] [ fortunesHead; body [] content ]

    let private fortunesTableHeader =
        tr [] [ th [] [ rawText "id" ]; th [] [ rawText "message" ] ]

    let fortunes (fortunes: Fortune seq) =
        [
            table [] [
                yield fortunesTableHeader
                for f in fortunes ->
                    tr [] [
                        td [] [ rawText <| string f.id ]
                        td [] [ encodedText <| f.message ]
                    ]
            ]
        ]
        |> layout

[<RequireQualifiedAccess>]
module HttpHandlers =
    open Giraffe
    open Giraffe.EndpointRouting
    open Giraffe.ViewEngine
    open Dapper
    open Npgsql

    let private extra = {
        id = 0
        message = "Additional fortune added at request time."
    }

    let private fortunes: HttpHandler =
        fun _ ctx ->
            task {
                use conn = new NpgsqlConnection (ConnectionString)

                let! data = conn.QueryAsync<Fortune> ("SELECT id, message FROM fortune")

                let view =
                    let xs = data.AsList ()
                    xs.Add extra
                    xs.Sort FortuneComparer
                    HtmlViews.fortunes xs

                let bytes = RenderView.AsBytes.htmlDocument view

                ctx.SetContentType "text/html;charset=utf-8"
                return! ctx.WriteBytesAsync bytes
            }

    let private db: HttpHandler =
        fun _ ctx ->
            task {
                let rnd = ctx.GetService<RandomUtil> ()
                use conn = new NpgsqlConnection (ConnectionString)

                let! data =
                    conn.QuerySingleAsync<World> (
                        "SELECT id, randomnumber FROM world WHERE id = @Id",
                        {| Id = rnd.Next () |}
                    )

                return! ctx.WriteJsonAsync data
            }

    let private queries: HttpHandler =
        fun _ ctx ->
            task {
                let queryParam =
                    ctx.TryGetQueryStringValue "queries"
                    |> Option.map (fun value ->
                        match System.Int32.TryParse value with
                        | true, intValue ->
                            if intValue < 1 then 1
                            elif intValue > 500 then 500
                            else intValue
                        | false, _ -> 1
                    )
                    |> Option.defaultValue 1

                let rnd = ctx.GetService<RandomUtil> ()

                let! res =
                    Array.init queryParam (fun _ -> rnd.Next ())
                    |> Array.map (fun id ->
                        use conn = new NpgsqlConnection (ConnectionString)

                        conn.QuerySingleAsync<World> (
                            "SELECT id, randomnumber FROM world WHERE id = @Id",
                            {| Id = id |}
                        )
                        |> Async.AwaitTask
                    )
                    |> fun computations ->
                        Async.Parallel (computations, MaxDegreeOfParallelism)

                return! ctx.WriteJsonAsync res
            }

    let endpoints: Endpoint list = [
        route "/plaintext" (text "Hello, World!")
        route "/json" (json {| message = "Hello, World!" |})
        route "/db" db
        route "/queries" queries
        route "/fortunes" fortunes
    ]


module Main =
    open Microsoft.AspNetCore.Builder
    open Microsoft.Extensions.DependencyInjection
    open Giraffe
    open Giraffe.EndpointRouting
    open Microsoft.Extensions.Hosting
    open Microsoft.Extensions.Logging
    open System.Text.Json
    open Newtonsoft.Json

    [<EntryPoint>]
    let main args =
        let jsonMode =
            match args with
            | [| "newtonsoft" |] -> Newtonsoft
            | [| "fsharpfriendly" |] -> FSharpFriendly
            | _ -> System

        printfn $"Running with %A{jsonMode} JSON serializer"

        let jsonSerializer =
            match jsonMode with
            | System -> Json.Serializer JsonSerializerOptions.Default :> Json.ISerializer
            | Newtonsoft ->
                NewtonsoftJson.Serializer (
                    JsonSerializerSettings (),
                    Microsoft.IO.RecyclableMemoryStreamManager ()
                )
                :> Json.ISerializer
            | FSharpFriendly -> Json.FsharpFriendlySerializer () :> Json.ISerializer

        let builder = WebApplication.CreateBuilder (args)

        let rnd = RandomUtil ()

        builder.Services
            .AddSingleton(jsonSerializer)
            .AddSingleton<RandomUtil>(rnd)
            .AddGiraffe ()
        |> ignore

        builder.Logging.ClearProviders () |> ignore

        let app = builder.Build ()

        app.UseRouting().UseGiraffe HttpHandlers.endpoints |> ignore

        app.Run ()

        0
