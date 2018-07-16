module EncodeHelper
open System.Text
open System.IO

// tempory writer
let StreamWrite (str:string,stream:Stream) =
    for char in str do
        if int char < 128 then // vast majority of encoding <128 
            stream.WriteByte(byte char)
        else
        for byte in Encoding.UTF8.GetBytes([|char|]) do
            stream.WriteByte byte
       

type JsonWrap<'T> =
    struct
        val Value : 'T
    new (v) = {Value=v}
    end

type XmlWrap<'T> =
    struct
        val Value : 'T
    new (v) = {Value=v}
    end

type TextWrap<'T> =
    struct
        val Value : 'T
    new (v) = {Value=v}
    end

let inline json (v:'T) : Stream -> unit = fun (ms:Stream) -> Utf8Json.JsonSerializer.Serialize<'T>(ms,v) 