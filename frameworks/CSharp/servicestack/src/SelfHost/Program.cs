using System;
using System.Collections.Generic;
using System.Linq;

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

                    // TODO: switch to Start after the next ServiceStack deployment (added to framework on commit #806)
                    appHost.StartListening(listeningOn);
                }
                catch (Exception ex)
                {
                    Console.WriteLine("ERROR: {0}: {1}", ex.GetType().Name, ex.Message);
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
