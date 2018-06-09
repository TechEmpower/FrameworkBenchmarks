// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Net;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Server.Kestrel.Core.Internal.Http;
using Microsoft.Extensions.Configuration;
using Npgsql;
using MySql.Data.MySqlClient;

namespace PlatformBenchmarks
{
    public class Program
    {
        public static string[] Args;

        public static void Main(string[] args)
        {
            Args = args;

            Console.WriteLine(BenchmarkApplication.ApplicationName);
            Console.WriteLine(BenchmarkApplication.Paths.Plaintext);
            Console.WriteLine(BenchmarkApplication.Paths.Json);
            Console.WriteLine(BenchmarkApplication.Paths.Fortunes);
            Console.WriteLine(BenchmarkApplication.Paths.SingleQuery);
            DateHeader.SyncDateTimer();

            BatchUpdateString.Initalize();

            BuildWebHost(args).Run();
        }

        public static IWebHost BuildWebHost(string[] args)
        {
            var config = new ConfigurationBuilder()
                .AddJsonFile("appsettings.json")
                .AddEnvironmentVariables(prefix: "ASPNETCORE_")
                .AddCommandLine(args)
                .Build();

            var appSettings = config.Get<AppSettings>();
            Console.WriteLine($"Database: {appSettings.Database}");

            if (appSettings.Database == DatabaseServer.PostgreSql)
            {
                BenchmarkApplication.Db = new RawDb(new ConcurrentRandom(), NpgsqlFactory.Instance, appSettings);
            }
            else if (appSettings.Database == DatabaseServer.MySql)
            {
                BenchmarkApplication.Db = new RawDb(new ConcurrentRandom(), MySqlClientFactory.Instance, appSettings);
            }

            var host = new WebHostBuilder()
                .UseBenchmarksConfiguration(config)
                .UseKestrel((context, options) =>
                {
                    IPEndPoint endPoint = context.Configuration.CreateIPEndPoint();

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
