module App.App

open System
open System.Text
open System.Text.Json.Serialization
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Dapper
open Giraffe
open Giraffe.ViewEngine
open Frank.Builder
open Npgsql
open Models

let inline contentLength x = new Nullable<int64> ( int64 x )

let options = JsonSerializerOptions()
options.Converters.Add(JsonFSharpConverter())

let json' : HttpContext -> Task =
    fun ctx ->
        ctx.Response.ContentType <- "application/json"
        ctx.Response.StatusCode <- 200
        let data = struct {|message="Hello, World!"|}
        JsonSerializer.SerializeAsync(ctx.Response.Body, data)

let text' (msg:string): HttpContext -> Task =
    let bytes = Encoding.UTF8.GetBytes(msg)
    fun ctx ->
        ctx.Response.ContentLength <- contentLength bytes.Length
        ctx.Response.ContentType <- "text/plain"
        ctx.Response.StatusCode <- 200
        ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)

// Pulled from Giraffe example
let fortunes' : HttpHandler = 
    fun _ ctx ->
        task {
            use conn = new NpgsqlConnection(ConnectionString)
            let! data = conn.QueryAsync<Fortune>("SELECT id, message FROM fortune")

            let view =
                let xs = data.AsList()
                xs.Add { id = 0; message = "Additional fortune added at request time." }
                xs.Sort FortuneComparer
                HtmlViews.fortunes xs

            let bytes = RenderView.AsBytes.htmlDocument view

            ctx.SetContentType "text/html;charset=utf-8"
            return! ctx.WriteBytesAsync bytes
        }

// Resources

let plaintext = 
    resource "/plaintext" {
        name "Plain text"
        get (text' "Hello, World!")
    }

let json =
    resource "/json" {
        name "JSON"
        get json'
    }

let fortunes =
    resource "/fortunes" {
        name "Fortunes"
        get fortunes'
    }

// App

[<EntryPoint>]
let main args = 
    webHost args {
        useDefaults
        service (fun services -> services.AddSingleton(options))
        configure (fun bldr ->
            bldr.ConfigureLogging(fun c -> c.ClearProviders() |> ignore)
                .UseKestrel())
        resource plaintext
        resource json
        resource fortunes
    }
    0
