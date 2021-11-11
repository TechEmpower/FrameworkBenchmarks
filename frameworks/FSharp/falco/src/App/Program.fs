module Program

open Falco
open App

[<Literal>]
let connectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3"

[<Literal>]
let defaultMsg = "Hello, World!"

type JsonModel = { message : string }

[<EntryPoint>]
let main args =        
    Host.startWebHost 
        args        
        (Server.configure connectionString)
        [
            get "/plaintext"  (Response.ofPlainText defaultMsg)
            get "/json"       (Response.ofJson { message = defaultMsg })
            get "/fortunes"   Fortune.handleIndex
        ]    
    0