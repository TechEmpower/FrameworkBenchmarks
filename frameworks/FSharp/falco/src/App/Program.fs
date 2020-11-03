module Program

open Falco
open App

[<Literal>]
let connectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3"

[<EntryPoint>]
let main args =        
    Host.startWebHost 
        args        
        (Host.configure connectionString)
        [
            get "/plaintext"  Value.handlePlainText
            get "/json"       Value.handleJson
            get "/fortunes"   Fortune.handleIndex
        ]    
    0