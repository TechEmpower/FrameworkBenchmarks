module App.Server

open System.Data
open Donald
open Falco
open Falco.Host
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Npgsql

type ConnectionString = string
type ConfigureLogging = ILoggingBuilder -> unit
type ConfigureServices = DbConnectionFactory -> IServiceCollection -> unit
type ConfigureApp = HttpEndpoint list -> IApplicationBuilder -> unit
type ConfigureServer = ConnectionString -> ConfigureWebHost

let configure : ConfigureServer =
    let configureLogging : ConfigureLogging =
        fun log ->
            log.ClearProviders()
            |> ignore

    let configureServices : ConfigureServices =
        fun connectionFactory services ->
            services
                .AddRouting() 
                .AddSingleton<DbConnectionFactory>(connectionFactory)
            |> ignore

    let configure : ConfigureApp =         
        fun endpoints app ->
            app.UseRouting()
               .UseHttpEndPoints(endpoints)       
               |> ignore 

    fun connectionString endpoints webHost ->    
        let connectionFactory =     
            fun () -> new NpgsqlConnection(connectionString) :> IDbConnection
    
        webHost
            .UseKestrel()
            .ConfigureLogging(configureLogging)
            .ConfigureServices(configureServices connectionFactory)
            .Configure(configure endpoints)
            |> ignore