namespace App

open System
open System.Collections.Generic

[<AutoOpen>]
module Common =

    [<Struct>]
    [<CLIMutable>]
    type JsonMessage = {
        message : string
    }

    [<CLIMutable>]
    type Fortune = {
        id: int
        message: string
    }

    [<Struct>]
    [<CLIMutable>]
    type World = {
        id: int
        randomnumber: int
    }

    [<Literal>]
    let ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3"

    let FortuneComparer = {
        new IComparer<Fortune> with
            member self.Compare(a,b) = String.CompareOrdinal(a.message, b.message)
    }
