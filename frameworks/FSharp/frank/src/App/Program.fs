module App.App

open System
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Dapper
open Frank.Builder
open FSharp.Control.Tasks
open Npgsql
open App
open Models

let [<Literal>] private DefaultCapacity = 1386
let private MaxBuilderSize = DefaultCapacity * 3

type MemoryStreamCache = 
    
    [<ThreadStatic>]
    [<DefaultValue>]
    static val mutable private instance: MemoryStream

    static member Get() = MemoryStreamCache.Get(DefaultCapacity)
    static member Get(capacity:int) = 
        
        if capacity <= MaxBuilderSize then
            let ms = MemoryStreamCache.instance;
            let capacity = max capacity DefaultCapacity
            
            if ms <> null && capacity <= ms.Capacity then
                MemoryStreamCache.instance <- null;
                ms.SetLength 0L
                ms
            else
                new MemoryStream(capacity)
        else
            new MemoryStream(capacity)

    static member Release(ms:MemoryStream) = 
        if ms.Capacity <= MaxBuilderSize then
            MemoryStreamCache.instance <- ms

let inline contentLength x = new Nullable<int64> ( int64 x )

let json' data : HttpContext -> Task<unit> =
    fun ctx ->
        let bytes = Utf8Json.JsonSerializer.Serialize(data)
        ctx.Response.ContentLength <- contentLength bytes.Length
        ctx.Response.ContentType <- "application/json"
        ctx.Response.StatusCode <- 200
        task {
            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
        }

let text' (msg:string): HttpContext -> Task<unit> =
    let bytes = System.Text.Encoding.UTF8.GetBytes(msg)
    fun ctx ->
        ctx.Response.ContentLength <- contentLength bytes.Length
        ctx.Response.ContentType <- "text/plain"
        ctx.Response.StatusCode <- 200
        task {
            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
        }

// Pulled from Giraffe example
let fortunes' : HttpContext -> Task<unit> = 
    let extra = { id = 0; message = "Additional fortune added at request time." }
    fun ctx ->
        let conn = new NpgsqlConnection(ConnectionString)
        ctx.Response.RegisterForDispose conn
        task {
            let! data = conn.QueryAsync<Fortune>("SELECT id, message FROM fortune")

            let fortunes = 
                let xs = data.AsList()
                xs.Add extra
                xs.Sort FortuneComparer
                xs

            let html = MemoryStreamCache.Get()
            let view = fortunes |> HtmlViews.fortunes 
            StetefullRendering.renderHtmlToStream html view

            ctx.Response.ContentType <- "text/html;charset=utf-8"
            ctx.Response.ContentLength <- contentLength html.Length
            ctx.Response.StatusCode <- 200
            do! html.CopyToAsync ctx.Response.Body

            MemoryStreamCache.Release html
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
        get (json' struct {| message = "Hello, World!" |})
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
        configure (fun bldr ->
            bldr.ConfigureLogging(fun c -> c.ClearProviders() |> ignore)
                .UseKestrel())
        resource plaintext
        resource json
        resource fortunes
    }
    0
