module Custom

open Dapper
open Giraffe
open System
open Models
open Npgsql
open FSharp.Control.Tasks
open System.Text
open System.Buffers

let private DefaultCapacity = 1386
let private MaxBuilderSize = DefaultCapacity * 3

type StringBuilderCache = 
    
    [<ThreadStatic>]
    [<DefaultValue>]
    static val mutable private instance: StringBuilder

    static member Get() = StringBuilderCache.Get(DefaultCapacity)
    static member Get(capacity:int) = 
        
        if capacity <= MaxBuilderSize then
            let sb = StringBuilderCache.instance;
            let capacity = max capacity DefaultCapacity
            
            if sb <> null && capacity <= sb.Capacity then
                StringBuilderCache.instance <- null;
                sb.Clear()
            else
                new StringBuilder(capacity)
        else
            new StringBuilder(capacity)

    static member Release(sb: StringBuilder) = 
        if sb.Capacity <= MaxBuilderSize then
            StringBuilderCache.instance <- sb

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
        
        let inline renderHtml view : byte[] =
            let sb = StringBuilderCache.Get()
            GiraffeViewEngine.ViewBuilder.buildHtmlDocument sb view
            let chars = ArrayPool<char>.Shared.Rent sb.Length
            sb.CopyTo(0, chars, 0, sb.Length)
            let result = Encoding.UTF8.GetBytes(chars, 0, sb.Length)
            StringBuilderCache.Release sb
            ArrayPool<char>.Shared.Return chars
            result

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
                
                let view = fortunes |> HtmlViews.fortunes 
                
                let bytes = renderHtml view
                ctx.Response.ContentType <- "text/html;charset=utf-8"
                ctx.Response.ContentLength <- contentLength bytes.Length
                ctx.Response.StatusCode <- 200
                do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
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

