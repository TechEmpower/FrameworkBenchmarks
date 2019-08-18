// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.IO;
using System.Reflection;
using System.Runtime;
using System.Threading;
using Benchmarks.Configuration;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

namespace Benchmarks
{
    public class Program
    {
        public static string[] Args;

        public static void Main(string[] args)
        {
            Args = args;

            Console.WriteLine();
            Console.WriteLine("ASP.NET Core Benchmarks");
            Console.WriteLine("-----------------------");

            Console.WriteLine($"Current directory: {Directory.GetCurrentDirectory()}");
            Console.WriteLine($"WebHostBuilder loading from: {typeof(WebHostBuilder).GetTypeInfo().Assembly.Location}");

            var config = new ConfigurationBuilder()
                .AddJsonFile("hosting.json", optional: true)
                .AddEnvironmentVariables(prefix: "ASPNETCORE_")
                .AddCommandLine(args)
                .Build();

            var webHostBuilder = new WebHostBuilder()
                .UseContentRoot(Directory.GetCurrentDirectory())
                .UseConfiguration(config)
                .UseStartup<Startup>()
                .ConfigureServices(services => services
                    .AddSingleton(new ConsoleArgs(args))
                    .AddSingleton<IScenariosConfiguration, ConsoleHostScenariosConfiguration>()
                    .AddSingleton<Scenarios>()
                )
                .UseDefaultServiceProvider(
                    (context, options) => options.ValidateScopes = context.HostingEnvironment.IsDevelopment())
                .UseKestrel();

            var threadCount = GetThreadCount(config);

            webHostBuilder.UseSockets(x =>
            {
                if (threadCount > 0)
                {
                    x.IOQueueCount = threadCount;
                }

                Console.WriteLine($"Using Sockets with {x.IOQueueCount} threads");
            });

            var webHost = webHostBuilder.Build();

            Console.WriteLine($"Server GC is currently {(GCSettings.IsServerGC ? "ENABLED" : "DISABLED")}");

            webHost.Run();
        }

        private static int GetThreadCount(IConfigurationRoot config)
        {
            var threadCountValue = config["threadCount"];
            return threadCountValue == null ? -1 : int.Parse(threadCountValue);
        }
    }
}
