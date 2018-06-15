module Custom

open Dapper
open System.Text
open Giraffe.GiraffeViewEngine
open Giraffe
open System.Collections.Generic
open System.Net
open System
open Models
open Npgsql
open Microsoft.Extensions.Primitives
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http
open System.IO

module CustomRendering = 
    let rec writeNode (target: TextWriter) (isHtml : bool) (node : XmlNode)  =

        let inline add (str:string) (target: TextWriter) =
            target.Write str
            target

        let inline add' (str:string) (target: TextWriter) =
            target.Write str

        let writeStartElement target selfClosing (elemName, attributes : XmlAttribute array) : unit =

            let closingBracket =
                match selfClosing with
                | false -> ">"
                | true ->
                    match isHtml with
                    | false -> " />"
                    | true  -> ">"

            match attributes with
            | [||] -> 
                do target
                    |> add "<"
                    |> add elemName
                    |> add' closingBracket
            | _    ->
                do target |> add "<" |> add' elemName

                for attr in attributes do
                    match attr with
                    | KeyValue (k, v) -> 
                        target |> add " " |> add k |> add "=\"" |> add' (WebUtility.HtmlEncode v)
                    | Boolean k       -> 
                        target |> add " " |> add' k

                do target |> add' closingBracket

        let writeEndElement target (elemName, _) = 
            do target |> add "</" |> add elemName |> add' ">"

        let writeParentNode target (elem : XmlElement, nodes : XmlNode list) =
            do elem  |> writeStartElement target false
            do nodes |> List.iter (writeNode target isHtml)
            do elem  |> writeEndElement target

        match node with
        | EncodedText text      -> do target |> add' (WebUtility.HtmlEncode text)
        | RawText text          -> do target |> add' text
        | ParentNode (e, nodes) -> do writeParentNode target (e, nodes)
        | VoidElement e         -> do writeStartElement target true e
    
    let renderHtml' node = 
        let ms = new MemoryStream(2048)
        let sb = new StreamWriter(ms, Encoding.UTF8)
        sb.WriteLine "<!DOCTYPE html>"
        do writeNode sb true node
        sb.Flush()
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        ms

let application : HttpHandler = 
    
    let inline contentLength (x:int32) = new System.Nullable<int64>( int64 x )

    let comp = { new IComparer<Fortune> with 
        member x.Compare(a,b) = System.String.CompareOrdinal(a.Message, b.Message)
    }

    let json' data : HttpHandler =
        let bytes = Utf8Json.JsonSerializer.Serialize(data)
        fun _ ctx ->
            ctx.Response.ContentLength <- contentLength(bytes.Length)
            ctx.Response.ContentType <- "application/json"
            ctx.Response.StatusCode <- 200
            ctx.WriteBytesAsync bytes

    let text' (msg:string): HttpHandler = 
        let bytes = System.Text.Encoding.UTF8.GetBytes(msg)
        fun _ ctx ->
            ctx.Response.ContentLength <- contentLength(bytes.Length)
            ctx.Response.ContentType <- "text/plain"
            ctx.Response.StatusCode <- 200
            ctx.WriteBytesAsync bytes
  
    let fortunes' : HttpHandler = 
        let extra = {Id = 0; Message = "Additional fortune added at request time."}
        let contentType = StringValues("text/html; charset=UTF-8")
        
        fun (_ : HttpFunc) (ctx : HttpContext) ->
            task {
                use conn = new NpgsqlConnection(ConnectionString)
                do! conn.OpenAsync()
                let! data = conn.QueryAsync<Fortune>("SELECT * FROM Fortune")
                
                let list = 
                    let xs = data.AsList()
                    xs.Add extra
                    xs.Sort comp
                    xs

                let stream = 
                    list
                    |> HtmlViews.fortunes
                    |> CustomRendering.renderHtml'

                ctx.Response.Headers.["Content-Type"] <- contentType
                return! ctx.WriteStreamAsync false stream None None
            }

    let notFound = setStatusCode 404

    let table = [
        TokenRouter.route "/plaintext" ( text' "Hello, World!" )
        TokenRouter.route "/json" ( json' { JsonStructMessage.message = "Hello, World!" } )
        TokenRouter.route "/fortunes" ( fortunes' )
    ]

    GET >=> TokenRouter.router notFound table