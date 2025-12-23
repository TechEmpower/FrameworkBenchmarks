using System;
using System.Threading;
using System.Threading.Tasks;

using EffinitiveFramework.Core;

namespace Benchmarks
{

    public static class Program
    {
        private static readonly ManualResetEvent WaitEvent = new(false);

        public static async Task<int> Main(string[] args)
        {
            var app = EffinitiveApp.Create()
                                   .UsePort(8080)
                                   .MapEndpoints()
                                   .Build();

            try
            {
                AppDomain.CurrentDomain.ProcessExit += (_, __) =>
                {
                    WaitEvent.Set();
                };

                await app.RunAsync();

                WaitEvent.WaitOne();

                return 0;
            }
            catch (Exception e)
            {
                Console.WriteLine(e);

                return -1;
            }
        }

    }

}
