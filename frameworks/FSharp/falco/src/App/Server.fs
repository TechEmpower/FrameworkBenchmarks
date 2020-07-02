module App.Server

open Donald
open Falco
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Hosting

let routes = 
    [
        get "/plaintext"  Value.Controller.handlePlainText
        get "/json"       Value.Controller.handleJson
        get "/fortunes"   Fortune.Controller.handleIndex
    ]
    
module Config =
    let configureKestrel (k : KestrelServerOptions) =
        k.AddServerHeader <- false
        ()

    let configureLogging (log : ILoggingBuilder) =
        log.ClearProviders()
        |> ignore

    let configureServices 
        (connectionFactory : DbConnectionFactory) 
        (services : IServiceCollection) =
        services.AddRouting() 
                .AddSingleton<DbConnectionFactory>(connectionFactory)
        |> ignore

    let configure 
        (routes : HttpEndpoint list) 
        (app : IApplicationBuilder) = 
        app.UseRouting()
            .UseHttpEndPoints(routes)       
            .UseNotFoundHandler(setStatusCode 404)
            |> ignore 

let builderServer 
    (connectionFactory : DbConnectionFactory) 
    (webHost : IWebHostBuilder) =
    webHost
        .UseKestrel(Config.configureKestrel)
        .ConfigureLogging(Config.configureLogging)
        .ConfigureServices(Config.configureServices connectionFactory)
        .Configure(Config.configure routes)                   
        |> ignore

let startServer 
    (args : string[])    
    (connectionFactory : DbConnectionFactory) =
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHost(fun webHost -> builderServer connectionFactory webHost)
        .Build()
        .Run()  
