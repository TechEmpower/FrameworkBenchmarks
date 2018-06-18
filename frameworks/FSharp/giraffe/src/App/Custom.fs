module Custom

open App
open Dapper
open Giraffe
open System
open Models
open Npgsql
open FSharp.Control.Tasks
open System.IO

let private DefaultCapacity = 1386
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

let application : HttpHandler = 

    let inline contentLength x = new Nullable<int64> ( int64 x )

    let json' data : HttpHandler =
        let bytes = Utf8Json.JsonSerializer.Serialize(data)
        fun _ ctx -> 
            ctx.Response.ContentLength <- contentLength bytes.Length
            ctx.Response.ContentType <- "application/json"
            ctx.Response.StatusCode <- 200
            task {
                do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                return Some ctx
            }

    let text' (msg:string): HttpHandler = 
        let bytes = System.Text.Encoding.UTF8.GetBytes(msg)
        fun _ ctx ->
            ctx.Response.ContentLength <- contentLength bytes.Length
            ctx.Response.ContentType <- "text/plain"
            ctx.Response.StatusCode <- 200
            task {
                do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                return Some ctx
            }
  
    let fortunes' : HttpHandler = 
        let extra = { id = 0; message = "Additional fortune added at request time." }
        fun _ ctx ->
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
                return Some ctx
            }

    let routes' (routes: (string * HttpHandler) list) : HttpHandler = 
        let table = Map.ofList routes
        let notFound = setStatusCode 404

        let go key = 
            if table |> Map.containsKey key then
                table.[key]
            else
                notFound

        fun next ctx ->
            let path = ctx.Request.Path.Value
            let handler = go path
            handler next ctx

    routes' [
        "/plaintext", text' "Hello, World!"
        "/json", json' { JsonStructMessage.message = "Hello, World!" }
        "/fortunes", fortunes'
    ]

