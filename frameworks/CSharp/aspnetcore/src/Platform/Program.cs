// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;

namespace PlatformBenchmarks;

public class Program
{
    public static string[] Args;

    public static async Task Main(string[] args)
    {
        Args = args;

        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.ApplicationName));
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.Plaintext));
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.Json));
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.FortunesRaw));
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.SingleQuery));
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.Updates));
        Console.WriteLine(Encoding.UTF8.GetString(BenchmarkApplication.Paths.MultipleQueries));
        DateHeader.SyncDateTimer();

        var host = BuildWebHost(args);
        var config = (IConfiguration)host.Services.GetService(typeof(IConfiguration));

        try
        {
            await BenchmarkApplication.RawDb.PopulateCache();
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error trying to populate database cache: {ex}");
        }

        await host.RunAsync();
    }

    public static IWebHost BuildWebHost(string[] args)
    {
        Console.WriteLine($"BuildWebHost()");
        Console.WriteLine($"Args: {string.Join(' ', args)}");

        var config = new ConfigurationBuilder()
            .AddJsonFile("appsettings.json")
#if DEBUG
            .AddUserSecrets<Program>()
#endif
            .AddEnvironmentVariables()
            .AddEnvironmentVariables()
            .AddCommandLine(args)
            .Build();

        var appSettings = config.Get<AppSettings>();
        Console.WriteLine($"ConnectionString: {appSettings.ConnectionString}");

        BenchmarkApplication.RawDb = new RawDb(new ConcurrentRandom(), appSettings);

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
                options.UnsafePreferInlineScheduling = true;
            }
        });

        var host = hostBuilder.Build();

        return host;
    }
}
