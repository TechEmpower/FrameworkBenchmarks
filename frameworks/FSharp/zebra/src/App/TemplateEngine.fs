module TemplateViewEngine
open System.IO
open System.Text
open System.Net


type Stream with
    member x.Write(value:int) =
        let mutable v = value
        
        if v < 0 then
            v <- (~~~v) + 1
            x.WriteByte(byte '-')
        
        // for i in 0 .. 8 do
        //     if v >= divisors.[i] then Math.DivRem(v,divisors.[i],&v) |> byte |> ms.WriteByte
        let inline check divisor =
            if v >= divisor then byte '0' + (System.Math.DivRem(v,divisor,&v) |> byte) |> x.WriteByte

        check 1000000000
        check 100000000
        check 10000000
        check 1000000
        check 100000
        check 10000
        check 1000
        check 100
        check 10

        byte '0' + (v |> byte) |> x.WriteByte

    member x.Write(value:string) =
        for char in value do
            if int char < 128 then // vast majority of encoding <128 
                x.WriteByte(byte char)
            else
            for byte in Encoding.UTF8.GetBytes([|char|]) do
                x.WriteByte byte    


type CompiledNode<'T> =
| CText of byte []
| CAttr of ('T -> string * string)
| CBindStr of ('T -> string)
| CBindInt of ('T -> int)
| CBindIf of ('T -> bool) * CompiledNode<'T> [] * CompiledNode<'T> []
| CBindFor of ('T * Stream -> unit)

type XmlAttr<'T> =
| KeyValue of string * string
| BindAttr of ('T -> string * string)

type XmlNode<'T> =
| ParentNode of  string * XmlAttr<'T> list * XmlNode<'T> list
| VoidNode of  string * XmlAttr<'T> list 
| EncodedText of string
| RawText of string
| BindStr of ('T -> string)
| BindInt of ('T -> int)
| BindIf of ('T -> bool) * CompiledNode<'T> [] * CompiledNode<'T> []
| BindFor of ('T * Stream -> unit)

let inline toUTF8 (v:string) = Encoding.UTF8.GetBytes v

let writeFlush (sb:StringBuilder,acc:CompiledNode<'T> list) =
    if sb.Length > 0 
    then 
        let nacc = (sb.ToString() |> toUTF8 |> CText) :: acc
        sb.Clear() |> ignore
        nacc
    else acc

let private compile (raw:XmlNode<'T>) (prefix:string option) : CompiledNode<'T> [] =
    let rec go node (sb:StringBuilder) acc =
        match node with
        | ParentNode (name,attrs,children) ->
            let mutable acc = acc 
            sb.Append("<" + name) |> ignore
            for attr in attrs do
                match attr with
                | KeyValue (key,value) -> sb.Append(key + "=" + value) |> ignore
                | BindAttr (fn) -> 
                    acc <- CAttr fn :: writeFlush(sb,acc)
                //| add bool flag
            sb.Append ">" |> ignore

            for child in children do
                acc <- go child sb acc
            
            sb.Append("</" + name + ">") |> ignore
            acc
                
        | VoidNode (name,attrs) ->
            let mutable acc = acc 
            sb.Append("<" + name) |> ignore
            for attr in attrs do
                match attr with
                | KeyValue (key,value) -> sb.Append(key + "=" + value) |> ignore
                | BindAttr (fn) -> 
                    acc <- CAttr fn :: writeFlush(sb,acc)
                //| add bool flag            
            sb.Append(" />") |> ignore
            acc
        | EncodedText txt -> sb.Append (WebUtility.HtmlEncode txt) |> ignore ; acc
        | RawText txt    -> sb.Append txt |> ignore; acc
        | BindStr fn        -> CBindStr fn :: writeFlush(sb,acc)
        | BindInt fn        -> CBindInt fn :: writeFlush(sb,acc)
        | BindIf (p,t,f) -> CBindIf(p,t,f) :: writeFlush(sb,acc)
        | BindFor fn     -> CBindFor(fn) :: writeFlush(sb,acc)
    
    let sb = 
        match prefix with
        | Some pre -> StringBuilder(pre) // re-usable stringbuilder for building string parts
        | None -> StringBuilder()

    let acc = go raw sb [] // node list in reverse order so position backwards down array
    let acc' = writeFlush(sb,acc)
    let result = Array.zeroCreate<_>(acc'.Length)
    let rec roll (ls,index) =
        match ls, index with
        | [] , -1 -> ()
        | h :: t, i -> result.[i] <- h ; roll (t,i - 1)
        | _,_ -> failwith "unexpected unroll error"
    roll (acc',result.Length - 1)
    result

let rec processNodes (item:'T,sw:Stream,nodes:CompiledNode<'T> [] ) =
    for node in nodes do
        match node with                
        | CText v -> sw.Write(v,0,v.Length) //.Write v
        | CBindStr fn -> item |> fn |> WebUtility.HtmlEncode |> sw.Write
        | CBindInt fn -> item |> fn |> sw.Write
        | CAttr fn -> let key,value = fn item in sw.Write(key) ; sw.Write("=") ; sw.Write(value)  
        | CBindIf (pred,trueFns,falseFns) ->
            if pred item then
                processNodes(item,sw,trueFns)
            else
                processNodes(item,sw,falseFns)
        | CBindFor (fn) -> fn(item,sw)

let compileDoc (raw:XmlNode<'T>) = 
    compile raw (Some "<!DOCTYPE html>")

let bindFor<'T,'U> (enumFn:'T -> #seq<'U>) (template:XmlNode<'U>) =
    let compiledNodes = compile template None
    BindFor (fun (model:'T,sw:Stream) ->
        for item in enumFn model do
            processNodes(item,sw,compiledNodes)
    )

let bindIf<'T> (predicate:'T -> bool,trueTemplate:XmlNode<'T>,falseTemplate:XmlNode<'T>) =
    let trueNodes = compile trueTemplate None
    let falseNodes = compile falseTemplate None
    BindIf(predicate,trueNodes,falseNodes)

let inline bindStr<'T>(map:'T -> string) = BindStr(map)
let inline bindInt<'T>(map:'T -> int) = BindInt(map)

let inline html attrs children      = ParentNode("html",attrs,children )
let inline ``base`` attrs           = VoidNode("base",attrs )
let inline head attrs children      = ParentNode("head",attrs,children )
let inline link attrs               = VoidNode("link",attrs )
let inline meta attrs               = VoidNode("meta",attrs )
let inline style attrs children     = ParentNode("style",attrs,children )
let inline title attrs children     = ParentNode("title",attrs,children )
let inline body attrs children      = ParentNode("body",attrs,children )
let inline address attrs children   = ParentNode("address",attrs,children )
let inline article attrs children   = ParentNode("article",attrs,children )
let inline aside attrs children     = ParentNode("aside",attrs,children )
let inline footer attrs children    = ParentNode("footer",attrs,children )
let inline hgroup attrs children    = ParentNode("hgroup",attrs,children )
let inline h1 attrs children        = ParentNode("h1",attrs,children )
let inline h2 attrs children        = ParentNode("h2",attrs,children )
let inline h3 attrs children        = ParentNode("h3",attrs,children )
let inline h4 attrs children        = ParentNode("h4",attrs,children )
let inline h5 attrs children        = ParentNode("h5",attrs,children )
let inline h6 attrs children        = ParentNode("h6",attrs,children )
let inline header attrs children    = ParentNode("header",attrs,children )
let inline nav attrs children       = ParentNode("nav",attrs,children )
let inline section attrs children   = ParentNode("section",attrs,children )
let inline dd attrs children        = ParentNode("dd",attrs,children )
let inline div attrs children       = ParentNode("div",attrs,children )
let inline dl attrs children        = ParentNode("dl",attrs,children )
let inline dt attrs children        = ParentNode("dt",attrs,children )
let inline figcaption attrs children= ParentNode("figcaption",attrs,children )
let inline figure attrs children    = ParentNode("figure",attrs,children )
let inline hr attrs                 = VoidNode("hr",attrs )
let inline li attrs children        = ParentNode("li",attrs,children )
let inline main attrs children      = ParentNode("main",attrs,children )
let inline ol attrs children        = ParentNode("ol",attrs,children )
let inline p attrs children         = ParentNode("p",attrs,children )
let inline pre attrs children       = ParentNode("pre",attrs,children )
let inline ul attrs children        = ParentNode("ul",attrs,children )
let inline a attrs children         = ParentNode("a",attrs,children )
let inline abbr attrs children      = ParentNode("abbr",attrs,children )
let inline b attrs children         = ParentNode("b",attrs,children )
let inline bdi attrs children       = ParentNode("bdi",attrs,children )
let inline bdo attrs children       = ParentNode("bdo",attrs,children )
let inline br attrs                 = VoidNode("br",attrs )
let inline cite attrs children      = ParentNode("cite",attrs,children )
let inline code attrs children      = ParentNode("code",attrs,children )
let inline data attrs children      = ParentNode("data",attrs,children )
let inline dfn attrs children       = ParentNode("dfn",attrs,children )
let inline em attrs children        = ParentNode("em",attrs,children )
let inline i attrs children         = ParentNode("i",attrs,children )
let inline kbd attrs children       = ParentNode("kbd",attrs,children )
let inline mark attrs children      = ParentNode("mark",attrs,children )
let inline q attrs children         = ParentNode("q",attrs,children )
let inline rp attrs children        = ParentNode("rp",attrs,children )
let inline rt attrs children        = ParentNode("rt",attrs,children )
let inline rtc attrs children       = ParentNode("rtc",attrs,children )
let inline ruby attrs children      = ParentNode("ruby",attrs,children )
let inline s attrs children         = ParentNode("s",attrs,children )
let inline samp attrs children      = ParentNode("samp",attrs,children )
let inline small attrs children     = ParentNode("small",attrs,children )
let inline span attrs children      = ParentNode("span",attrs,children )
let inline strong attrs children    = ParentNode("strong",attrs,children )
let inline sub attrs children       = ParentNode("sub",attrs,children )
let inline sup attrs children       = ParentNode("sup",attrs,children )
let inline time attrs children      = ParentNode("time",attrs,children )
let inline u attrs children         = ParentNode("u",attrs,children )
let inline var attrs children       = ParentNode("var",attrs,children )
let inline wbr attrs                = VoidNode("wbr",attrs )
let inline area attrs               = VoidNode("area",attrs )
let inline audio attrs children     = ParentNode("audio",attrs,children )
let inline img attrs                = VoidNode("img",attrs )
let inline map attrs children       = ParentNode("map",attrs,children )
let inline track attrs              = VoidNode("track",attrs )
let inline video attrs children     = ParentNode("video",attrs,children )
let inline embed attrs              = VoidNode("embed",attrs )
let inline object attrs children    = ParentNode("object",attrs,children )
let inline param attrs              = VoidNode("param",attrs )
let inline source attrs             = VoidNode("source",attrs )
let inline canvas attrs children    = ParentNode("canvas",attrs,children )
let inline noscript attrs children  = ParentNode("noscript",attrs,children )
let inline script attrs children    = ParentNode("script",attrs,children )
let inline del attrs children       = ParentNode("del",attrs,children )
let inline ins attrs children       = ParentNode("ins",attrs,children )
let inline caption attrs children   = ParentNode("caption",attrs,children )
let inline col attrs                = VoidNode("col",attrs )
let inline colgroup attrs children  = ParentNode("colgroup",attrs,children )
let inline table attrs children     = ParentNode("table",attrs,children )
let inline tbody attrs children     = ParentNode("tbody",attrs,children )
let inline td attrs children        = ParentNode("td",attrs,children )
let inline tfoot attrs children     = ParentNode("tfoot",attrs,children )
let inline th attrs children        = ParentNode("th",attrs,children )
let inline thead attrs children     = ParentNode("thead",attrs,children )
let inline tr attrs children        = ParentNode("tr",attrs,children )
let inline button attrs children    = ParentNode("button",attrs,children )
let inline datalist attrs children  = ParentNode("datalist",attrs,children )
let inline fieldset attrs children  = ParentNode("fieldset",attrs,children )
let inline form attrs children      = ParentNode("form",attrs,children )
let inline input attrs              = VoidNode("input",attrs )
let inline label attrs children     = ParentNode("label",attrs,children )
let inline legend attrs children    = ParentNode("legend",attrs,children )
let inline meter attrs children     = ParentNode("meter",attrs,children )
let inline optgroup attrs children  = ParentNode("optgroup",attrs,children )
let inline option attrs children    = ParentNode("option",attrs,children )
let inline output attrs children    = ParentNode("output",attrs,children )
let inline progress attrs children  = ParentNode("progress",attrs,children )
let inline select attrs children    = ParentNode("select",attrs,children )
let inline textarea attrs children  = ParentNode("textarea",attrs,children )
let inline details attrs children   = ParentNode("details",attrs,children )
let inline dialog attrs children    = ParentNode("dialog",attrs,children )
let inline menu attrs children      = ParentNode("menu",attrs,children )
let inline menuitem attrs           = VoidNode("menuitem",attrs )
let inline summary attrs children   = ParentNode("summary",attrs,children )
let inline encodedText txt          = EncodedText(txt)
let inline rawText txt              = RawText(txt)
let inline comment txt              = RawText("<!-- " + txt + " -->")


let inline renderHtmlDocument (model:'T) ( document : CompiledNode<'T> []) (writer : #Stream) =
    processNodes(model,writer,document) 