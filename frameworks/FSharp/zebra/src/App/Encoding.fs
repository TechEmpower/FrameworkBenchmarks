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