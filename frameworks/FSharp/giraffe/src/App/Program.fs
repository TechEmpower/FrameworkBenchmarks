namespace App

[<AutoOpen>]
module Common =
    open System
    open System.Collections.Generic

    [<CLIMutable>]
    type Fortune =
        {
            id      : int
            message : string
        }

    [<Literal>]
    let ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000"

    type JsonMode =
        | System
        | Utf8
        | Newtonsoft

    let FortuneComparer =
        {
            new IComparer<Fortune> with
                member __.Compare (a, b) =
                    String.CompareOrdinal(a.message, b.message)
        }

[<RequireQualifiedAccess>]
module HtmlViews =
    open Giraffe.ViewEngine

    let private fortunesHead =
        head [] [
            title []  [ rawText "Fortunes" ]
        ]

    let private layout (content: XmlNode list) =
        html [] [
            fortunesHead
            body [] content
        ]

    let private fortunesTableHeader =
        tr [] [
            th [] [ rawText "id" ]
            th [] [ rawText "message" ]
        ]

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
        ] |> layout

[<RequireQualifiedAccess>]
module HttpHandlers =
    open Giraffe
    open Giraffe.EndpointRouting
    open Giraffe.ViewEngine
    open Dapper
    open Npgsql

    let private extra =
        {
            id      = 0
            message = "Additional fortune added at request time."
        }

    let private fortunes : HttpHandler =
        fun _ ctx ->
            task {
                use conn = new NpgsqlConnection(ConnectionString)
                let! data = conn.QueryAsync<Fortune>("SELECT id, message FROM fortune")

                let view =
                    let xs = data.AsList()
                    xs.Add extra
                    xs.Sort FortuneComparer
                    HtmlViews.fortunes xs

                let bytes = RenderView.AsBytes.htmlDocument view

                ctx.SetContentType "text/html;charset=utf-8"
                return! ctx.WriteBytesAsync bytes
            }

    let endpoints : Endpoint list =
        [
            route "/plaintext" (text "Hello, World!")
            route "/json" (json {| message = "Hello, World!" |})
            route "/fortunes" fortunes
        ]


module Main =
    open Microsoft.AspNetCore.Builder
    open Microsoft.AspNetCore.Hosting
    open Microsoft.Extensions.DependencyInjection
    open Giraffe
    open Giraffe.EndpointRouting
    open Microsoft.Extensions.Hosting
    open Microsoft.Extensions.Logging

    [<EntryPoint>]    
    let main args =
        let jsonMode =
            match args with
            | [| "newtonsoft" |] -> Newtonsoft
            | [| "utf8" |]       -> Utf8
            | _                  -> System

        printfn $"Running with %A{jsonMode} JSON serializer"

        let jsonSerializer =
            match jsonMode with
            | System ->
                SystemTextJson.Serializer(SystemTextJson.Serializer.DefaultOptions)
                :> Json.ISerializer
            | Utf8 ->
                Utf8Json.Serializer(Utf8Json.Serializer.DefaultResolver)
                :> Json.ISerializer
            | Newtonsoft ->
                NewtonsoftJson.Serializer(NewtonsoftJson.Serializer.DefaultSettings)
                :> Json.ISerializer

        let builder = WebApplication.CreateBuilder(args)

        builder.Services
            .AddSingleton(jsonSerializer)
            .AddGiraffe() |> ignore
            
        builder.Logging.ClearProviders() |> ignore

        let app = builder.Build()

        app.UseRouting()
           .UseGiraffe HttpHandlers.endpoints |> ignore

        app.Run()
        
        0