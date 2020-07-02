module App.Value

open Falco 

type JsonOutputModel = { message : string }
    
module Controller =
    let defaultMsg = "Hello, World!"

    let defaultJson = { message = defaultMsg }

    let handleJson : HttpHandler =
        jsonOut defaultJson
        
    let handlePlainText : HttpHandler =
        textOut defaultMsg