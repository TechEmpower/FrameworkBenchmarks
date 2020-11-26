module Stock

open Giraffe
open Giraffe.ViewEngine
open Dapper
open Npgsql
open Models
open FSharp.Control.Tasks

let extra = {id = 0; message = "Additional fortune added at request time."}

let fortunes : HttpHandler = 
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

let application : HttpHandler = 
    choose [
        route "/plaintext" >=> text "Hello, World!" 
        route "/json" >=> json {| message = "Hello, World!" |}
        route "/fortunes" >=> fortunes
    ]
