module App.Host

open System.Data
open Donald
open Falco
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Npgsql

let configureServices 
    (connectionFactory : DbConnectionFactory) 
    (services : IServiceCollection) =
    services.AddRouting() 
            .AddSingleton<DbConnectionFactory>(connectionFactory)
    |> ignore

let configureApp
    (endpoints : HttpEndpoint list) 
    (app : IApplicationBuilder) =      
    app.UseRouting()
        .UseHttpEndPoints(endpoints)       
        .UseNotFoundHandler(Response.withStatusCode 404 >> Response.ofPlainText "Not Found")
        |> ignore 

let configure (connectionString : string) =
    let connectionFactory =     
        fun () -> new NpgsqlConnection(connectionString) :> IDbConnection
    
    fun (endpoints : HttpEndpoint list)
        (webHost : IWebHostBuilder) ->
        webHost
            .UseKestrel()
            .ConfigureLogging(fun log -> log.ClearProviders() |> ignore)
            .ConfigureServices(configureServices connectionFactory)
            .Configure(configureApp endpoints)                   
            |> ignore
