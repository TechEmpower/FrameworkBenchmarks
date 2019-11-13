using System;
using System.Threading;
using System.Threading.Tasks;

using GenHTTP.Core;
using GenHTTP.Modules.Core;
using GenHTTP.Modules.Webservices;

using Benchmarks.Tests;

namespace Benchmarks
{

    public static class Program
    {

        public static int Main(string[] args)
        {
            try
            {
                var waitEvent = new AutoResetEvent(false);

                AppDomain.CurrentDomain.ProcessExit += (s, e) =>
                {
                    waitEvent.Set();
                };

                var tests = Layout.Create()
                                  .Add("plaintext", Content.From("Hello, World!"))
                                  .Add<JsonResource>("json");

                var server = Server.Create()
                                   .Router(tests)
                                   .Compression(false);

                using (var instance = server.Build())
                {
                    waitEvent.WaitOne();
                }

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
