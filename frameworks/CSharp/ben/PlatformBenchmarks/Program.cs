// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Runtime.InteropServices;
using System.Threading.Tasks;

using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
#if DATABASE
using Npgsql;
#endif

using PlatformExtensions;

namespace PlatformBenchmarks
{
    public class Program
    {
        public static async Task Main(string[] args)
        {
            Console.WriteLine("ASP.NET Core Platform Level");
            foreach (var path in BenchmarkApplication.Paths.Enabled)
            {
                Console.WriteLine(path);
            }

            var host = BuildWebHost(args);
            var config = (IConfiguration)host.Services.GetService(typeof(IConfiguration));
            BatchUpdateString.Database = config.Get<AppSettings>().Database;
#if DATABASE
            await BenchmarkApplication.Db.PopulateCache();
#endif
            await host.RunAsync();
        }

        public static IWebHost BuildWebHost(string[] args)
        {
            var config = new ConfigurationBuilder()
                .AddJsonFile("appsettings.json")
                .AddEnvironmentVariables()
                .AddEnvironmentVariables(prefix: "ASPNETCORE_")
                .AddCommandLine(args)
                .Build();

            var appSettings = config.Get<AppSettings>();
#if DATABASE
            Console.WriteLine($"Database: {appSettings.Database}");
            Console.WriteLine($"ConnectionString: {appSettings.ConnectionString}");

            if (appSettings.Database == DatabaseServer.PostgreSql)
            {
                BenchmarkApplication.Db = new RawDb(new ConcurrentRandom(), appSettings);
            }
            else
            {
                throw new NotSupportedException($"{appSettings.Database} is not supported");
            }
#endif

            var host = new WebHostBuilder()
                .UseSockets(options =>
                {
                    options.WaitForDataBeforeAllocatingBuffer = false;
                    if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
                    {
                        options.UnsafePreferInlineScheduling = Environment.GetEnvironmentVariable("DOTNET_SYSTEM_NET_SOCKETS_INLINE_COMPLETIONS") == "1";
                    }
                })
                .UseKestrel((context, options) =>
                {
                    var endPoint = context.Configuration.CreateIPEndPoint();

                    options.Listen(endPoint, builder =>
                    {
                        builder.UseHttpApplication<BenchmarkApplication>();
                    });
                })
                .UseStartup<Program>()
                .Build();

            return host;
        }

        public void Configure(IApplicationBuilder app)
        {

        }
    }
}
