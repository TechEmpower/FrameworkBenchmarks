module App.Server

open System.Threading.Tasks
open Donald
open Falco
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging

let buildServer (connectionFactory : DbConnectionFactory) : Host.ConfigureWebHost =
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
        let handleNotFound : HttpHandler =
            fun ctx ->
                Response.withStatusCode 404 ctx |> ignore
                Task.CompletedTask
            
        app.UseRouting()
           .UseHttpEndPoints(routes)       
           .UseNotFoundHandler(handleNotFound)
            |> ignore 

    fun (routes : HttpEndpoint list)
        (webHost : IWebHostBuilder) ->
        webHost
            .UseKestrel()
            .ConfigureLogging(configureLogging)
            .ConfigureServices(configureServices connectionFactory)
            .Configure(configure routes)                   
            |> ignore
