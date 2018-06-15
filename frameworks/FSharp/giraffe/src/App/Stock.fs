module Stock

open Giraffe
open Dapper
open Npgsql
open Microsoft.AspNetCore.Http
open Models
open Microsoft.Extensions.Primitives
open System.Text
open FSharp.Control.Tasks

let application : HttpHandler = 
    let contentType = StringValues [|"text/html"; "charset=UTF-8"|]

    let fortunes : HttpHandler = 
        let extra = {Id = 0; Message = "Additional fortune added at request time."}
        let extra' = Seq.singleton extra

        fun (_ : HttpFunc) (ctx : HttpContext) ->
            task {
                use conn = new NpgsqlConnection(ConnectionString)
                do! conn.OpenAsync()
                let! data = conn.QueryAsync<Fortune>("SELECT * FROM Fortune")
                        
                let utf8 = 
                    data 
                    |> Seq.append extra' 
                    |> Seq.sortBy (fun x -> x.Message)

                    |> HtmlViews.fortunes
                    |> GiraffeViewEngine.renderHtmlDocument

                    |> Encoding.UTF8.GetBytes

                ctx.Response.Headers.["Content-Type"] <- contentType
                return! ctx.WriteBytesAsync utf8
            }

    GET >=> choose [
        route "/plaintext" >=> text "Hello, World!" 
        route "/json" >=> json { JsonMessage.message = "Hello, World!" }
        route "/fortunes" >=> fortunes
    ]
