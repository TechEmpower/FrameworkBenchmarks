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

let application : HttpHandler = 
    
    let inline contentLength x = new Nullable<_> ( int64 x )

    let comp = { new IComparer<Fortune> with 
        member self.Compare(a,b) = String.CompareOrdinal(a.message, b.message)
    }

    let none: Option<HttpContext> = None

    let json' data : HttpHandler =
        let bytes = Utf8Json.JsonSerializer.Serialize(data)
        fun _ ctx -> 
            ctx.Response.ContentLength <- contentLength bytes.Length
            ctx.Response.ContentType <- "application/json"
            ctx.Response.StatusCode <- 200
            task {
                do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                return none
            }

    let text' (msg:string): HttpHandler = 
        let bytes = System.Text.Encoding.UTF8.GetBytes(msg)
        fun _ ctx ->
            ctx.Response.ContentLength <- contentLength bytes.Length
            ctx.Response.ContentType <- "text/plain"
            ctx.Response.StatusCode <- 200
            task {
                do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
                return none
            }
  
    let fortunes' : HttpHandler = 
        let extra = {id = 0; message = "Additional fortune added at request time."}
        let encoding = new UTF8Encoding(false)

        fun (_ : HttpFunc) (ctx : HttpContext) ->
            
                let conn = new NpgsqlConnection(ConnectionString)
                ctx.Response.RegisterForDispose conn
                task {
                    let! data = conn.QueryAsync<Fortune>("SELECT id, message FROM fortune")
                
                    let fortunes = 
                        let xs = data.AsList()
                        xs.Add extra
                        xs.Sort comp
                        xs

                    let html = 
                        fortunes
                        |> HtmlViews.fortunes
                        |> StetefullRendering.renderHtmlToStream encoding

                    ctx.Response.ContentType <- "text/html;charset=utf-8"
                    ctx.Response.ContentLength <- contentLength html.Length
                    ctx.Response.StatusCode <- 200
                    do! html.CopyToAsync ctx.Response.Body
                    return none
                }

    let table = [
        route "/plaintext" >=> ( text' "Hello, World!" )
        route "/json" >=> ( json' { JsonStructMessage.message = "Hello, World!" } )
        route "/fortunes" >=> ( fortunes' )
    ]

    choose table