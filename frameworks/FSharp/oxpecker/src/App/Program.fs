namespace App

open System
open Oxpecker

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

    let fortunes (fortunesData: ResizeArray<Fortune>) =
        table() {
            fortunesTableHeader
            for fortune in fortunesData do
                tr() {
                    td() { raw <| string fortune.id }
                    td() { fortune.message }
                }
        } |> layout

[<RequireQualifiedAccess>]
module HttpHandlers =
    open System.Text
    open Microsoft.AspNetCore.Http
    open System.Text.Json

    let private extra =
        {
            id      = 0
            message = "Additional fortune added at request time."
        }

    let rec private renderFortunes (ctx: HttpContext) (data: ResizeArray<Fortune>) =
        data.Add extra
        data.Sort FortuneComparer
        data |> HtmlViews.fortunes |> ctx.WriteHtmlViewChunked

    let fortunes : EndpointHandler =
        fun ctx ->
            task {
                let! dbFortunes = loadFortunes ()
                return! renderFortunes ctx dbFortunes
            }

    let singleQuery : EndpointHandler =
        fun ctx ->
            task {
                let! result = loadSingleRow()
                return! ctx.WriteJsonChunked result
            }

    let private parseQueries (ctx: HttpContext) =
        match ctx.TryGetRouteValue<string>("count") with
        | Some q ->
            match Int32.TryParse q with
            | true, q when q > 1 -> if q < 500 then q else 500
            | _, _ -> 1
        | _ -> 1

    let multipleQueries : EndpointHandler =
        fun ctx ->
            let count = parseQueries ctx
            task {
                let! results = loadMultipleRows count
                return! ctx.WriteJsonChunked results
            }

    let multipleUpdates : EndpointHandler =
        fun ctx ->
            let count = parseQueries ctx
            task {
                let! results = doMultipleUpdates count
                return! ctx.WriteJsonChunked results
            }

    let utf8Const (s: string): EndpointHandler =
        let result = s |> Encoding.UTF8.GetBytes
        fun ctx ->
            ctx.SetContentType("text/plain")
            ctx.WriteBytes(result)

    let jsonSimple value : EndpointHandler =
        let options = JsonSerializerOptions(JsonSerializerDefaults.Web)
        fun ctx ->
            ctx.Response.WriteAsJsonAsync(value, options)

    let endpoints =
        [|
            route "/plaintext" <| utf8Const "Hello, World!"
            route "/json" <| jsonSimple { message = "Hello, World!" }
            route "/fortunes" fortunes
            route "/db" singleQuery
            route "/queries/{count?}" multipleQueries
            route "/updates/{count?}" multipleUpdates
        |]

module Main =
    open Microsoft.AspNetCore.Builder
    open Microsoft.Extensions.DependencyInjection
    open Microsoft.Extensions.Hosting
    open Microsoft.Extensions.Logging

    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder(args)
        builder.Services
            .AddRouting()
            .AddOxpecker() |> ignore
        builder.Logging
            .ClearProviders() |> ignore
        let app = builder.Build()
        app
            .UseRouting()
            .UseOxpecker(HttpHandlers.endpoints) |> ignore
        app.Run()
        0