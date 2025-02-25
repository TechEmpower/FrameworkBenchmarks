module Program

open System.Data
open Falco
open Falco.Markup
open Falco.Routing
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Logging
open Npgsql

[<Literal>]
let connectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000"

[<Literal>]
let defaultMsg = "Hello, World!"

type JsonResponse =
    { message : string }

type Fortune =
    { id : int
      message : string }

    static member Default =
        { id = 0
          message = "Additional fortune added at request time." }

let handleFortunes (connStr : string) : HttpHandler = fun ctx -> task {
    use conn = new NpgsqlConnection(connStr)

    use comd = conn.CreateCommand()
    comd.CommandText <- "SELECT id, message FROM fortune"

    do! conn.OpenAsync()
    use! redr = comd.ExecuteReaderAsync(CommandBehavior.SequentialAccess)

    let! dbFortunes =
        task {
            let mutable shouldContinue = true
            let fortunes = ResizeArray<Fortune>()

            while shouldContinue do
                let! fortunesRead = redr.ReadAsync()

                if not fortunesRead then
                    shouldContinue <- false
                else
                    fortunes.Add { id = redr.GetInt32(0)
                                   message = redr.GetString(1) }
            return fortunes |> List.ofSeq
        }

    redr.Dispose()
    comd.Dispose()
    conn.Dispose()

    let sortedFortunes =
        Fortune.Default ::
        dbFortunes
        |> List.sortBy (fun f -> f.message)

    let html =
        Elem.html [] [
            Elem.head [] [
                    Elem.title [] [ Text.raw "Fortunes" ]
                ]
            Elem.body [] [
                Elem.table [] [
                        yield Elem.tr [] [
                                Elem.th [] [ Text.raw "id" ]
                                Elem.th [] [ Text.raw "message" ]
                            ]
                        for fortune in sortedFortunes ->
                            Elem.tr [] [
                                    Elem.td [] [ Text.raw (string fortune.id) ]
                                    Elem.td [] [ Text.enc fortune.message]
                                ]
                    ]
            ]
        ]

    return Response.ofHtml html ctx
}

[<EntryPoint>]
let main args =
    let bldr  = WebApplication.CreateBuilder(args)
    bldr.Logging.ClearProviders() |> ignore

    let wapp = bldr.Build()

    wapp.UseRouting()
        .UseFalco([
            get "/plaintext" (Response.ofPlainText defaultMsg)
            get "/json" (Response.ofJson { message = defaultMsg })
            get "/fortunes" (handleFortunes connectionString)
        ])
        .Run()
    0