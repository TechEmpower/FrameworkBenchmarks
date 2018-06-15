module Custom

open App
open Dapper
open Giraffe
open System.Collections.Generic
open System
open Models
open Npgsql
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http
open System.Text
open System 

let application : HttpHandler = 
    
    let inline contentLength x = new Nullable<_> ( int64 x )

    let comp = { new IComparer<Fortune> with 
        member self.Compare(a,b) = String.CompareOrdinal(a.Message, b.Message)
    }

    let json' data : HttpHandler =
        let bytes = Utf8Json.JsonSerializer.Serialize(data)
        fun _ ctx ->
            ctx.Response.ContentLength <- contentLength bytes.Length
            ctx.Response.ContentType <- "application/json"
            ctx.Response.StatusCode <- 200
            ctx.WriteBytesAsync bytes

    let text' (msg:string): HttpHandler = 
        let bytes = System.Text.Encoding.UTF8.GetBytes(msg)
        fun _ ctx ->
            ctx.Response.ContentLength <- contentLength bytes.Length
            ctx.Response.ContentType <- "text/plain"
            ctx.Response.StatusCode <- 200
            ctx.WriteBytesAsync bytes
  
    let fortunes' : HttpHandler = 
        let extra = {Id = 0; Message = "Additional fortune added at request time."}
        let encoding = new UTF8Encoding(false)

        fun (_ : HttpFunc) (ctx : HttpContext) ->
            task {
                let conn = new NpgsqlConnection(ConnectionString)
                ctx.Response.RegisterForDispose conn

                let! data = conn.QueryAsync<Fortune>("SELECT * FROM Fortune")
                
                let list = 
                    let xs = data.AsList()
                    xs.Add extra
                    xs.Sort comp
                    xs

                let stream = 
                    list
                    |> HtmlViews.fortunes
                    |> StetefullRendering.renderHtmlToStream encoding

                ctx.Response.ContentType <- "text/html"
                ctx.Response.ContentLength <- contentLength stream.Length
                ctx.Response.StatusCode <- 200
                do! stream.CopyToAsync ctx.Response.Body
                return None
            }

    let table = [
        route "/plaintext" >=> ( text' "Hello, World!" )
        route "/json" >=> ( json' { JsonStructMessage.message = "Hello, World!" } )
        route "/fortunes" >=> ( fortunes' )
    ]

    GET >=> choose table