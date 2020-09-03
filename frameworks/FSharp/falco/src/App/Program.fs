module App.Program

open System.Data
open Falco
open Npgsql

[<Literal>]
let ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3"

let connectionFactory =     
    fun () -> new NpgsqlConnection(ConnectionString) :> IDbConnection

[<EntryPoint>]
let main args =    
    Host.startWebHost 
        args        
        (Server.buildServer connectionFactory)
        [
            get "/plaintext"  Value.handlePlainText
            get "/json"       Value.handleJson
            get "/fortunes"   Fortune.handleIndex
        ]    
    0