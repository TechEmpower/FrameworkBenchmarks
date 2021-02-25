using EmbedIO;
using Swan.Logging;
using System;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Benchmarks
{

    #region Supporting data structures

    public class JsonResult
    {

        public string Message { get; set; }

    }

    #endregion

    public static class Program
    {
        private static readonly ManualResetEvent _WaitEvent = new ManualResetEvent(false);

        private static readonly Encoding _Utf8NoBom = new UTF8Encoding(false);

        public static async Task<int> Main(string[] args)
        {
            Logger.UnregisterLogger<ConsoleLogger>();

            using var server = new WebServer(o => o
                  .WithUrlPrefix("http://+:8080/")
                  .WithMode(HttpListenerMode.EmbedIO))
                .PreferNoCompressionFor("text/*")
                .WithAction("/plaintext", HttpVerbs.Get, ctx =>
                    ctx.SendStringAsync("Hello, World!", "text/plain", _Utf8NoBom))
                .WithAction("/json", HttpVerbs.Get, ctx =>
                {
                    var data = new JsonResult() { Message = "Hello, World!" };

                    return ctx.SendDataAsync(ResponseSerializer.Json, data);
                });

            try
            {
                AppDomain.CurrentDomain.ProcessExit += (_, __) =>
                {
                    _WaitEvent.Set();
                };

                await server.RunAsync();

                _WaitEvent.WaitOne();

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
