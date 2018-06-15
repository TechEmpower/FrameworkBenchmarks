module App.App

open Microsoft.AspNetCore.Hosting
open Giraffe

[<EntryPoint>]
let main args = 
    let implementation = 
        match args with
        | [| "stock" |] -> Models.Stock
        | _ -> Models.Custom

    printfn "Running with %A implementation" implementation

    let webApp = function
    | Models.Custom -> Custom.application
    | Models.Stock -> Stock.application

    WebHostBuilder()
        .UseKestrel()
        .Configure(fun app -> app.UseGiraffe (webApp implementation))
        .ConfigureServices(fun services -> services.AddGiraffe() |> ignore)
        .Build()
        .Run()
    0