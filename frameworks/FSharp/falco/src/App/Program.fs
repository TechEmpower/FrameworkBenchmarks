module Program

open System.Data
open Dapper
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

let handleFortunes (connStr : string) : HttpHandler = fun ctx -> task {

    use conn = new NpgsqlConnection(connStr)
    let! data = conn.QueryAsync<Fortune>("SELECT id, message FROM fortune")
    let fortunes = data.AsList()
    fortunes.Add({ id = 0
                   message = "Additional fortune added at request time." })

    let sortedFortunes =
        fortunes
        |> Seq.sortBy (fun f -> f.message)

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