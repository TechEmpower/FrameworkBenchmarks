namespace App
open System.Text
open Giraffe.GiraffeViewEngine
open System.Net
open System.IO

module rec StetefullRendering =

    let private UTF8WithoutBOM = new UTF8Encoding(false)

    let inline private add (str:string) (target: StreamWriter) =
        target.Write str
        target

    let inline private add' (str:string) (target: StreamWriter) =
        target.Write str
    
    let private closingBracket = ">"

    let private writeStartElement target elemName (attributes : XmlAttribute array) : unit =

        match attributes with
        | [||] -> 
            target
            |> add "<"
            |> add elemName
            |> add' closingBracket

        | _  ->
            target 
            |> add "<" 
            |> add' elemName

            for attr in attributes do
                match attr with
                | KeyValue (k, v) -> 
                    target 
                    |> add " " 
                    |> add k 
                    |> add "=\"" 
                    |> add' (WebUtility.HtmlEncode v)

                | Boolean k -> 
                    target 
                    |> add " " 
                    |> add' k

            target 
            |> add' closingBracket

    let private writeEndElement target elemName = 
        target 
        |> add "</" 
        |> add elemName 
        |> add' ">"

    let private writeParentNode target ((name, attrs) : XmlElement) (nodes : XmlNode list) =
        writeStartElement target name attrs 
        nodes |> List.iter (writeHtmlNode target)
        name  |> writeEndElement target

    let rec private writeHtmlNode (target: StreamWriter) (node : XmlNode)  =
        match node with
        | EncodedText text -> target |> add' (WebUtility.HtmlEncode text)
        | RawText text -> target |> add' text
        | ParentNode (e, nodes) -> writeParentNode target e nodes
        | VoidElement (n, attrs) -> writeStartElement target n attrs
    
    let renderHtmlToStream (ms:MemoryStream) node = 
        let sb = new StreamWriter(ms, UTF8WithoutBOM)
        sb.WriteLine "<!DOCTYPE html>"
        writeHtmlNode sb node
        sb.Flush()
        ms.Seek(0L, SeekOrigin.Begin) |> ignore

    let renderHtml node =
        let ms = new MemoryStream()
        renderHtmlToStream ms node
        ms