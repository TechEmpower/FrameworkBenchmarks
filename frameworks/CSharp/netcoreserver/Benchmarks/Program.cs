using System;
using System.Net;
using System.Threading;

namespace Benchmarks
{

    public static class Program
    {

        private static readonly ManualResetEvent _WaitEvent = new ManualResetEvent(false);

        public static int Main(string[] args)
        {
            var server = new HttpBenchmarkServer(IPAddress.Any, 8080);

            try
            {
                AppDomain.CurrentDomain.ProcessExit += (_, __) =>
                {
                    _WaitEvent.Set();
                };

                server.Start();

                _WaitEvent.WaitOne();

                return 0;
            }
            catch (Exception e)
            {
                Console.WriteLine(e);

                return -1;
            }
            finally
            {
                server.Stop();
            }
        }

    }

}
