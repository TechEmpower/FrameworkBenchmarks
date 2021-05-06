using System;
using System.Linq;
using System.Net;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

using WatsonWebserver;

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

        public static async Task<int> Main(string[] args)
        {
#if DEBUG
            var host = "127.0.0.1";
#else
            var host = "tfb-server";
#endif

            using var server = new Server(host, 8080, false, DefaultRoute);

            server.Routes.Static.Add(HttpMethod.GET, "/plaintext", PlaintextRoute);
            server.Routes.Static.Add(HttpMethod.GET, "/json", JsonRoute);

            try
            {
                AppDomain.CurrentDomain.ProcessExit += (_, __) =>
                {
                    _WaitEvent.Set();
                };

                await server.StartAsync();

                _WaitEvent.WaitOne();

                return 0;
            }
            catch (Exception e)
            {
                Console.WriteLine(e);

                return -1;
            }
        }

        static async Task DefaultRoute(HttpContext ctx)
        {
            ctx.Response.StatusCode = 404;
            ctx.Response.StatusDescription = "Not Found";

            await ctx.Response.Send("Not found.");
        }

        static async Task PlaintextRoute(HttpContext ctx)
        {
            ctx.Response.Headers.Add("Content-Type", "text/plain; charset=UTF-8");

            await ctx.Response.Send("Hello, World!");
        }

        static async Task JsonRoute(HttpContext ctx)
        {
            var response = new JsonResult() { Message = "Hello, World!" };
            var serialized = JsonSerializer.Serialize(response);

            ctx.Response.Headers.Add("Content-Type", "application/json; charset=UTF-8");

            await ctx.Response.Send(serialized);
        }

    }

}
