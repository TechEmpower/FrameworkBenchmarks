module App.Value

open Falco 

type JsonOutputModel = { message : string }

let defaultMsg = "Hello, World!"

let handleJson : HttpHandler =
    { message = defaultMsg }
    |> Response.ofJson
        
let handlePlainText : HttpHandler =        
    Response.ofPlainText defaultMsg