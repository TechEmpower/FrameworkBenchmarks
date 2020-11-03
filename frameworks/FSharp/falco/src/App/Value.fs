module App.Value

open Falco 

let defaultMsg = "Hello, World!"

let handleJson : HttpHandler =
    fun ctx ->
        let output = {| message = defaultMsg |}        
        Response.ofJson output ctx
        
let handlePlainText : HttpHandler =        
    Response.ofPlainText defaultMsg