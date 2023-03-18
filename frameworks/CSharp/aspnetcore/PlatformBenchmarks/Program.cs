// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Runtime.InteropServices;
using System.Text;

namespace PlatformBenchmarks;

public sealed class Program
{
    public static string[] Args;

    public static async Task Main(string[] args)
    {
        Args = args;

#if NPGSQL
        // This disables SQL parsing/rewriting, which requires using positional parameters and NpgsqlBatch everywhere.
        // This helps commands where there are no parameters (Fortunes); when there are parameters, their ParameterName
        // being null already triggers positional parameters and disables parsing)
        // Note that Dapper and EF aren't yet compatible with this mode.
        AppContext.SetSwitch("Npgsql.EnableSqlRewriting", false);
#endif

        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.ApplicationName));
#if !DATABASE
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.Plaintext));
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.Json));
#else
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.Fortunes));
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.SingleQuery));
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.Updates));
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.MultipleQueries));
#endif
        DateHeader.SyncDateTimer();

        var host = BuildWebHost(args);
        var config = (IConfiguration)host.Services.GetService(typeof(IConfiguration));
        BatchUpdateString.DatabaseServer = config.Get<AppSettings>().Database;
#if DATABASE
        try
        {
            await BenchmarkApplication.Db.PopulateCache();
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error trying to populate database cache: {ex}");
        }
#endif
        await host.RunAsync();
    }

    public static IWebHost BuildWebHost(string[] args)
    {
        var config = new ConfigurationBuilder()
            .AddJsonFile("appsettings.json")
#if DEBUG
            .AddUserSecrets<Program>()
#endif
            .AddEnvironmentVariables()
            .AddEnvironmentVariables(prefix: "ASPNETCORE_")
            .AddCommandLine(args)
            .Build();

        var appSettings = config.Get<AppSettings>();
#if DATABASE
        Console.WriteLine($"Database: {appSettings.Database}");
        Console.WriteLine($"ConnectionString: {appSettings.ConnectionString}");

        if (appSettings.Database is DatabaseServer.PostgreSql
                                 or DatabaseServer.MySql)
        {
            BenchmarkApplication.Db = new RawDb(new ConcurrentRandom(), appSettings);
        }
        else
        {
            throw new NotSupportedException($"{appSettings.Database} is not supported");
        }
#endif

        var hostBuilder = new WebHostBuilder()
            .UseBenchmarksConfiguration(config)
            .UseKestrel((context, options) =>
            {
                var endPoint = context.Configuration.CreateIPEndPoint();

                options.Listen(endPoint, builder =>
                {
                    builder.UseHttpApplication<BenchmarkApplication>();
                });
            })
            .UseStartup<Startup>();

        hostBuilder.UseSockets(options =>
        {
            options.WaitForDataBeforeAllocatingBuffer = false;

            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            {
                options.UnsafePreferInlineScheduling = Environment.GetEnvironmentVariable("DOTNET_SYSTEM_NET_SOCKETS_INLINE_COMPLETIONS") == "1";
            }
        });


        var host = hostBuilder.Build();

        return host;
    }
}
