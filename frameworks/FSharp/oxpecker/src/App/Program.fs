namespace App

[<AutoOpen>]
module Common =
    open System

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
    open Oxpecker
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

    let endpoints : Endpoint[] =
        [|
            route "/plaintext" <| text "Hello, World!"
            route "/json"<| jsonChunked {| message = "Hello, World!" |}
            route "/fortunes" fortunes
        |]


module Main =
    open Microsoft.AspNetCore.Builder
    open Microsoft.AspNetCore.Hosting
    open Microsoft.Extensions.DependencyInjection
    open Oxpecker
    open Microsoft.Extensions.Hosting
    open Microsoft.Extensions.Logging

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)

        builder.Services
            .AddRouting()
            .AddOxpecker() |> ignore

        builder.Logging.ClearProviders() |> ignore
        builder.WebHost.ConfigureKestrel(fun options -> options.AllowSynchronousIO <- true) |> ignore

        let app = builder.Build()

        app.UseRouting()
           .UseOxpecker HttpHandlers.endpoints |> ignore

        app.Run()

        0