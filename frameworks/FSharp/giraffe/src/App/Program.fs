module App.App

open Microsoft.AspNetCore.Hosting
open Giraffe
open Models
open Microsoft.Extensions.DependencyInjection
open Giraffe.Serialization

[<EntryPoint>]
let main args = 
    let implementation = 
        match args with
        | [| "stock" |] -> Implementation.Stock
        | _ -> Implementation.Custom

    printfn "Running with %A implementation" implementation

    let webApp = function
    | Implementation.Custom -> Custom.application
    | Implementation.Stock -> Stock.application

    let app = webApp implementation

    WebHostBuilder()
        .UseKestrel()
        .Configure(fun b -> b.UseGiraffe app)
        .ConfigureServices(fun s ->
            s.AddGiraffe() |> ignore
            s.AddSingleton<IJsonSerializer>(Utf8JsonSerializer(Utf8JsonSerializer.DefaultResolver)) |> ignore
        )
        .Build()
        .Run()
    0