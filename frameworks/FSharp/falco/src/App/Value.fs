module App.Value

open Falco 

type JsonOutputModel = { message : string }
    
module Controller =
    let defaultMsg = "Hello, World!"

    let handleJson : HttpHandler =
        fun next ctx ->
            let json = { message = defaultMsg }
            (jsonOut json) next ctx
        
    let handlePlainText : HttpHandler =        
        textOut defaultMsg