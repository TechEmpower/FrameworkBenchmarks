// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.IO;
using System.Runtime;
using System.Threading;
using Benchmarks.Configuration;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks
{
    public class Program
    {
        public static string[] Args;
        public static string Server;

        public static void Main(string[] args)
        {
            Args = args;

            Console.WriteLine();
            Console.WriteLine("ASP.NET Core Benchmarks");
            Console.WriteLine("-----------------------");

            Console.WriteLine($"Current directory: {Directory.GetCurrentDirectory()}");

            var config = new ConfigurationBuilder()
                .AddCommandLine(args)
                .AddEnvironmentVariables(prefix: "ASPNETCORE_")
                .AddJsonFile("hosting.json", optional: true)
                .Build();

            Server = config["server"] ?? "Kestrel";

            var webHostBuilder = new WebHostBuilder()
                .UseContentRoot(Directory.GetCurrentDirectory())
                .UseConfiguration(config)
                .UseStartup<Startup>()
                .ConfigureServices(services => services
                    .AddSingleton(new ConsoleArgs(args))
                    .AddSingleton<IScenariosConfiguration, ConsoleHostScenariosConfiguration>()
                    .AddSingleton<Scenarios>()
                );

            if (String.Equals(Server, "Kestrel", StringComparison.OrdinalIgnoreCase))
            {
                var threads = GetThreadCount(config);
                webHostBuilder = webHostBuilder.UseKestrel((options) =>
                {
                    if (threads > 0)
                    {
                        options.ThreadCount = threads;
                    }
                });
            }
            else if (String.Equals(Server, "WebListener", StringComparison.OrdinalIgnoreCase))
            {
                webHostBuilder = webHostBuilder.UseWebListener();
            }
            else
            {
                throw new InvalidOperationException($"Unknown server value: {Server}");
            }

            var webHost = webHostBuilder.Build();

            Console.WriteLine($"Using server {Server}");
            Console.WriteLine($"Server GC is currently {(GCSettings.IsServerGC ? "ENABLED" : "DISABLED")}");

            var nonInteractiveValue = config["NonInteractive"];
            if (nonInteractiveValue == null || !bool.Parse(nonInteractiveValue))
            {
                StartInteractiveConsoleThread();
            }

            webHost.Run();
        }

        private static void StartInteractiveConsoleThread()
        {
            // Run the interaction on a separate thread as we don't have Console.KeyAvailable on .NET Core so can't
            // do a pre-emptive check before we call Console.ReadKey (which blocks, hard)

            var started = new ManualResetEvent(false);

            var interactiveThread = new Thread(() =>
            {
                Console.WriteLine("Press 'C' to force GC or any other key to display GC stats");
                Console.WriteLine();

                started.Set();

                while (true)
                {
                    var key = Console.ReadKey(intercept: true);

                    if (key.Key == ConsoleKey.C)
                    {
                        Console.WriteLine();
                        Console.Write("Forcing GC...");
                        GC.Collect();
                        GC.WaitForPendingFinalizers();
                        GC.Collect();
                        Console.WriteLine(" done!");
                    }
                    else
                    {
                        Console.WriteLine();
                        Console.WriteLine($"Allocated: {GetAllocatedMemory()}");
                        Console.WriteLine($"Gen 0: {GC.CollectionCount(0)}, Gen 1: {GC.CollectionCount(1)}, Gen 2: {GC.CollectionCount(2)}");
                    }
                }
            });

            interactiveThread.IsBackground = true;
            interactiveThread.Start();

            started.WaitOne();
        }

        private static string GetAllocatedMemory(bool forceFullCollection = false)
        {
            double bytes = GC.GetTotalMemory(forceFullCollection);

            return $"{((bytes / 1024d) / 1024d).ToString("N2")} MB";
        }

        private static int GetThreadCount(IConfigurationRoot config)
        {
            var threadCountValue = config["threadCount"];
            return threadCountValue == null ? -1 : int.Parse(threadCountValue);
        }
    }
}

