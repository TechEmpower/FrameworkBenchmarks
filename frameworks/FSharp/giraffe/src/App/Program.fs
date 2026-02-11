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
        Maximum Pool Size=1024;
        NoResetOnClose=true;
        Enlist=false;
        Max Auto Prepare=4
        """

    [<Literal>]
    let MultiplexedConnectionString = ConnectionString + ";Multiplexing=true"

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

    // Cache SQL strings for bulk updates to avoid rebuilding on every request
    // This module generates and caches SQL UPDATE statements that use PostgreSQL's
    // VALUES clause to update multiple rows in a single statement.
    module BatchUpdateSql =
        let private cache = Array.zeroCreate<string> 501

        let get (count: int) =
            match cache.[count] with
            | null ->
                let lastIndex = count - 1

                // Build the VALUES clause: (@Id_0, @Rn_0), (@Id_1, @Rn_1), ...
                // Each pair represents (id, new_randomnumber) for one row
                let valueClauses =
                    List.init lastIndex (fun i -> sprintf "(@Id_%d, @Rn_%d), " i i)
                    |> String.concat ""

                // The final SQL uses a CTE-like VALUES construct to update multiple rows:
                // UPDATE world SET randomnumber = temp.randomnumber
                // FROM (VALUES ...) AS temp(id, randomnumber)
                // WHERE temp.id = world.id
                let sql =
                    sprintf
                        """
                        UPDATE world
                        SET randomnumber = temp.randomnumber
                        FROM (VALUES %s(@Id_%d, @Rn_%d) ORDER BY 1)
                        AS temp(id, randomnumber)
                        WHERE temp.id = world.id
                        """
                        valueClauses
                        lastIndex
                        lastIndex

                cache.[count] <- sql
                sql
            | sql -> sql

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

    let fortunes: HttpHandler =
        fun _ ctx ->
            task {
                let dataSource = ctx.GetService<NpgsqlDataSource> ()
                use conn = dataSource.CreateConnection ()
                do! conn.OpenAsync ()

                let! data = conn.QueryAsync<Fortune> ("SELECT id, message FROM fortune")

                let view =
                    let xs = data.AsList ()
                    xs.Add { id = 0; message = "Additional fortune added at request time." }
                    xs.Sort FortuneComparer
                    HtmlViews.fortunes xs

                let bytes = RenderView.AsBytes.htmlDocument view

                ctx.SetContentType "text/html;charset=utf-8"
                return! ctx.WriteBytesAsync bytes
            }

    let db: HttpHandler =
        fun _ ctx ->
            task {
                let rnd = ctx.GetService<RandomUtil> ()
                let dataSource = ctx.GetService<NpgsqlDataSource> ()
                use conn = dataSource.CreateConnection ()
                do! conn.OpenAsync ()

                let! data =
                    conn.QuerySingleAsync<World> (
                        "SELECT id, randomnumber FROM world WHERE id = @Id",
                        {| Id = rnd.Next () |}
                    )

                return! ctx.WriteJsonAsync data
            }

    let queries: HttpHandler =
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
                let dataSource = ctx.GetService<NpgsqlDataSource> ()

                use conn = dataSource.CreateConnection ()
                do! conn.OpenAsync ()

                // Read all rows sequentially
                let results = Array.zeroCreate<World> queryParam

                for i in 0 .. queryParam - 1 do
                    let! world =
                        conn.QuerySingleAsync<World> (
                            "SELECT id, randomnumber FROM world WHERE id = @Id",
                            {| Id = rnd.Next () |}
                        )

                    results.[i] <- world

                return! ctx.WriteJsonAsync results
            }

    let updates: HttpHandler =
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

                // Use multiplexed connection for updates (more efficient for sequential ops)
                use conn = new NpgsqlConnection (MultiplexedConnectionString)
                do! conn.OpenAsync ()

                // Read all rows sequentially
                let readResults = Array.zeroCreate<World> queryParam

                for i in 0 .. queryParam - 1 do
                    let! world =
                        conn.QuerySingleAsync<World> (
                            "SELECT id, randomnumber FROM world WHERE id = @Id",
                            {| Id = rnd.Next () |}
                        )

                    readResults.[i] <- world

                // Update random numbers functionally
                let updatedData =
                    readResults
                    |> Array.map (fun data -> { data with randomNumber = rnd.Next () })

                // Build bulk update parameters functionally
                // We use a single UPDATE statement with a VALUES clause to update all rows at once.
                //
                // Example SQL for 2 rows:
                // UPDATE world SET randomnumber = temp.randomnumber
                // FROM (VALUES (@Id_0, @Rn_0), (@Id_1, @Rn_1) ORDER BY 1) AS temp(id, randomnumber)
                // WHERE temp.id = world.id
                let updateParams =
                    updatedData
                    |> Array.mapi (fun i data -> [
                        sprintf "@Id_%d" i, box data.id // Parameter for the id
                        sprintf "@Rn_%d" i, box data.randomNumber // Parameter for the new random number
                    ])
                    |> Array.collect List.toArray // Flatten the list of parameter pairs
                    |> dict // Convert to dictionary for Dapper

                // Execute bulk update using Dapper
                let sql = BatchUpdateSql.get queryParam
                let! _ = conn.ExecuteAsync (sql, updateParams)

                return! ctx.WriteJsonAsync updatedData
            }

    let endpoints: Endpoint list = [
        route "/json" (json {| message = "Hello, World!" |})
        route "/db" db
        route "/queries" queries
        route "/fortunes" fortunes
        route "/updates" updates
        route "/plaintext" (text "Hello, World!")
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
    open Npgsql

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
            .AddSingleton<NpgsqlDataSource>(NpgsqlDataSource.Create (ConnectionString))
            .AddGiraffe ()
        |> ignore

        builder.Logging.ClearProviders () |> ignore

        let app = builder.Build ()

        app.UseRouting().UseGiraffe HttpHandlers.endpoints |> ignore

        app.Run ()

        0
