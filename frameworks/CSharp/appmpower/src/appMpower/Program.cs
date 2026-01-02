using System;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Hosting.Server.Features;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using Microsoft.Extensions.Logging;

namespace appMpower;

class Program
{
    static void Main(string[] args)
    {
        // ------------------------------------------------------------
        // 1) Load configuration files (independent of ASP.NET host)
        // ------------------------------------------------------------
        IConfigurationRoot fileConfig = new ConfigurationBuilder()
            .AddJsonFile("appsettings.json", optional: false, reloadOnChange: true)
            .Build();

        // Use config values as defaults when env vars not provided
        ApplyDatabaseDefaults(fileConfig);

        // ------------------------------------------------------------
        // 2) Create builder
        // ------------------------------------------------------------
        var builder = WebApplication.CreateSlimBuilder(args);

        // Hardening: prevent external hosting startup + built-in startup filters
        builder.WebHost.UseSetting(WebHostDefaults.PreventHostingStartupKey, "true");
        builder.Services.RemoveAll<IStartupFilter>();

        // Logging (keep simple)
        builder.Logging.ClearProviders();
        //builder.Logging.AddConsole();

        // ------------------------------------------------------------
        // 3) Compose builder.Configuration (merge file config + env + CLI)
        // ------------------------------------------------------------
        builder.Configuration.AddConfiguration(fileConfig);
        builder.Configuration.AddEnvironmentVariables(prefix: "ASPNETCORE_");
        builder.Configuration.AddCommandLine(args);

        var startup = new Startup();
        startup.ConfigureServices(builder.Services, builder.Configuration);

        // ------------------------------------------------------------
        // 4) Build
        // ------------------------------------------------------------
        var app = builder.Build();

        // ------------------------------------------------------------
        // 5) Pipeline
        // ------------------------------------------------------------

        // Your existing Startup pipeline
        startup.Configure(app);

        // ------------------------------------------------------------
        // 6) Log addresses after startup
        // ------------------------------------------------------------
        app.Lifetime.ApplicationStarted.Register(() =>
        {
            var addresses = app.Services
                .GetRequiredService<Microsoft.AspNetCore.Hosting.Server.IServer>()
                .Features
                .Get<IServerAddressesFeature>();

            if (addresses is not null)
            {
                foreach (var address in addresses.Addresses)
                    Console.WriteLine($"🚀 Listening on {address}");
            }
        });

        // ------------------------------------------------------------
        // 7) Run
        // ------------------------------------------------------------
        app.Run();
    }

    private static void ApplyDatabaseDefaults(IConfiguration config)
    {
#if !DEBUG
    #if ODBC      
        NativeMethods.DbProvider(1); //ODBC
    #else
        NativeMethods.DbProvider(0); //ADO
    #endif        

    #if POSTGRESQL      
        NativeMethods.Dbms(1); //PostgreSQL
    #else
        NativeMethods.Dbms(0); //MySQL
    #endif
#endif
    }
}