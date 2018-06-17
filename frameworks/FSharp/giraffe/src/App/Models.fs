module Models

open System.Collections.Generic
open System

type JsonMessage = { message : string }

[<Struct>]
type JsonStructMessage = { message : string }

 [<CLIMutable>]
type Fortune = { id: int; message: string }

[<Literal>]
let ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3"

type Implementation = Stock | Custom

let FortuneComparer = { new IComparer<Fortune> with 
    member self.Compare(a,b) = String.CompareOrdinal(a.message, b.message)
}