namespace App
module rec StetefullRendering =

    open System.Text
    open Giraffe.GiraffeViewEngine
    open System.Net
    open System.IO

    let inline private add (str:string) (target: StreamWriter) =
        target.Write str
        target

    let inline private add' (str:string) (target: StreamWriter) =
        target.Write str

    let private closingBracket = ">"

    let private writeStartElement target elemName (attributes : XmlAttribute array) : unit =

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
                | Boolean k -> 
                    target |> add " " |> add' k

            do target |> add' closingBracket

    let private writeEndElement target elemName = 
        do target |> add "</" |> add elemName |> add' ">"

    let private writeParentNode target ((name, attrs) : XmlElement) (nodes : XmlNode list) =
        do writeStartElement target name attrs 
        do nodes |> List.iter (writeHtmlNode target)
        do name  |> writeEndElement target

    let rec writeHtmlNode (target: StreamWriter) (node : XmlNode)  =
        match node with
        | EncodedText text -> do target |> add' (WebUtility.HtmlEncode text)
        | RawText text -> do target |> add' text
        | ParentNode (e, nodes) -> do writeParentNode target e nodes
        | VoidElement (n, attrs) -> do writeStartElement target n attrs
    
    let renderHtmlToStream (encoding:Encoding) node = 
        let ms = new MemoryStream()
        let sb = new StreamWriter(ms, encoding)
        do sb.WriteLine "<!DOCTYPE html>"
        do writeHtmlNode sb node
        do sb.Flush()
        do ms.Seek(0L, SeekOrigin.Begin) |> ignore
        ms