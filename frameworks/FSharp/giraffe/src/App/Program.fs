module App.App

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.AspNetCore.Http

// ---------------------------------
// Models
// ---------------------------------

type JsonMessage = { message : string }

// ---------------------------------
// Web app
// ---------------------------------

let jsonUtf8 (data:obj) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let bytes = Utf8Json.JsonSerializer.Serialize(data)
        ctx.SetContentType "application/json"
        ctx.WriteBytesAsync bytes


let webApp =
    choose [
        GET >=>
            choose [
                route "/plaintext" >=> text "Hello, World!"
                route "/json" >=> json { message = "Hello, World!" }
                route "/jsonutf8" >=> jsonUtf8 { message = "Hello, World!" }
            ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true  -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)      
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =    
    services.AddGiraffe() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    let filter (l : LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =    
    WebHostBuilder()
        .UseKestrel()        
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0