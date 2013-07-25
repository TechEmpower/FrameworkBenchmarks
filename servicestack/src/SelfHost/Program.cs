using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

using ServiceStackBenchmark;

namespace ServiceStackBenchmark.SelfHost
{
    class Program
    {
        static void Main(string[] args)
        {
            var listeningOn = args.Length == 0 ? "http://*:1337/" : args[0];

            using (var appHost = new AppSelfHost())
            {
                try
                {
                    appHost.Init();
                    appHost.Start(listeningOn);

                    Console.WriteLine("AppHost Created at {0}, listening on {1}", DateTime.Now, listeningOn);
                    Console.WriteLine("Press <CTRL>+C to stop.");
                    Thread.Sleep(Timeout.Infinite);
                }
                catch (Exception ex)
                {
                    Console.WriteLine("ERROR: {0}: {1}", ex.GetType().Name, ex.Message);
                    throw;
                }
                finally
                {
                    appHost.Stop();
                }
            }

            Console.WriteLine("AppHost has finished");
        }
    }
}
