﻿// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Net;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
#if DATABASE
using Npgsql;
#endif

namespace PlatformBenchmarks
{
    public class Program
    {
        public static string[] Args;

        public static async Task Main(string[] args)
        {
            Utf8Json.Resolvers.CompositeResolver.RegisterAndSetAsDefault(    
                Utf8Json.Resolvers.GeneratedResolver.Instance);

            Args = args;

            Console.WriteLine(BenchmarkApplication.ApplicationName);
#if !DATABASE
            Console.WriteLine(BenchmarkApplication.Paths.Plaintext);
            Console.WriteLine(BenchmarkApplication.Paths.Json);
#else
            Console.WriteLine(BenchmarkApplication.Paths.Fortunes);
            Console.WriteLine(BenchmarkApplication.Paths.SingleQuery);
            Console.WriteLine(BenchmarkApplication.Paths.Updates);
            Console.WriteLine(BenchmarkApplication.Paths.MultipleQueries);
#endif
            DateHeader.SyncDateTimer();

            var host = BuildWebHost(args);
#if DATABASE
            var config = (IConfiguration)host.Services.GetService(typeof(IConfiguration));
            BatchUpdateString.DatabaseServer = config.Get<AppSettings>().Database;
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

#if DATABASE
            var appSettings = config.Get<AppSettings>();
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
                .UseBenchmarksConfiguration(config)
                .UseKestrel((context, options) =>
                {
                    var endPoint = context.Configuration.CreateIPEndPoint();

                    options.Listen(endPoint, builder =>
                    {
                        builder.UseHttpApplication<BenchmarkApplication>();
                    });
                })
                .UseStartup<Startup>()
                .Build();

            return host;
        }
    }
}
