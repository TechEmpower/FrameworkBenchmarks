module Stock

open Giraffe
open Dapper
open Npgsql
open Microsoft.AspNetCore.Http
open Models
open FSharp.Control.Tasks

let application : HttpHandler = 
    
    let fortunes : HttpHandler = 
        let extra = {Id = 0; Message = "Additional fortune added at request time."}
        let extra' = Seq.singleton extra

        fun (_ : HttpFunc) (ctx : HttpContext) ->
            task {
                use conn = new NpgsqlConnection(ConnectionString)
                let! data = conn.QueryAsync<Fortune>("SELECT * FROM Fortune")

                let view = 
                    data 
                    |> Seq.append extra'
                    |> Seq.sortBy (fun x -> x.Message)
                    |> HtmlViews.fortunes

                return! ctx.WriteHtmlViewAsync(view)
            }

    GET >=> choose [
        route "/plaintext" >=> text "Hello, World!" 
        route "/json" >=> json { JsonMessage.message = "Hello, World!" }
        route "/fortunes" >=> fortunes
    ]
