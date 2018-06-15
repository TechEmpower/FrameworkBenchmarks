module Stock

open Giraffe
open Dapper
open Npgsql
open Microsoft.AspNetCore.Http
open Models
open FSharp.Control.Tasks
open System.Text

let application : HttpHandler = 
    
    let fortunes : HttpHandler = 
        let extra = {Id = 0; Message = "Additional fortune added at request time."}
        let extra' = Seq.singleton extra

        fun (_ : HttpFunc) (ctx : HttpContext) ->
            task {
                use conn = new NpgsqlConnection(ConnectionString)
                let! data = conn.QueryAsync<Fortune>("SELECT id, message FROM fortune")

                let view = 
                    data 
                    |> Seq.append extra'
                    |> Seq.sortBy (fun x -> x.Message)
                    |> HtmlViews.fortunes

                // stock implementation does not allow to set content type for view rendering in 1.1.0
                let bytes = 
                    view 
                    |> GiraffeViewEngine.renderHtmlDocument 
                    |> Encoding.UTF8.GetBytes

                ctx.SetContentType "text/html;charset=utf-8"
                return! ctx.WriteBytesAsync bytes
            }

    GET >=> choose [
        route "/plaintext" >=> text "Hello, World!" 
        route "/json" >=> json { JsonMessage.message = "Hello, World!" }
        route "/fortunes" >=> fortunes
    ]
