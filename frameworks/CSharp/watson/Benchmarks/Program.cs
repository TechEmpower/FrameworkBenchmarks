using System;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

using WatsonWebserver;
using WatsonWebserver.Core;

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
        private static readonly ManualResetEvent WaitEvent = new(false);

        public static async Task<int> Main(string[] args)
        {
#if DEBUG
            var host = "127.0.0.1";
#else
            var host = "tfb-server";
#endif

            var settings = new WebserverSettings(host, 8080, false);

            using var server = new Webserver(settings, DefaultRoute);

            server.Routes.PreAuthentication.Static.Add(HttpMethod.GET, "/plaintext", PlaintextRoute);
            server.Routes.PreAuthentication.Static.Add(HttpMethod.GET, "/json", JsonRoute);

            try
            {
                AppDomain.CurrentDomain.ProcessExit += (_, __) =>
                {
                    WaitEvent.Set();
                };

                await server.StartAsync();

                WaitEvent.WaitOne();

                return 0;
            }
            catch (Exception e)
            {
                Console.WriteLine(e);

                return -1;
            }
        }

        static async Task DefaultRoute(HttpContextBase ctx)
        {
            ctx.Response.StatusCode = 404;

            await ctx.Response.Send("Not found.");
        }

        static async Task PlaintextRoute(HttpContextBase ctx)
        {
            ctx.Response.Headers.Add("Content-Type", "text/plain; charset=UTF-8");

            await ctx.Response.Send("Hello, World!");
        }

        static async Task JsonRoute(HttpContextBase ctx)
        {
            var response = new JsonResult() { Message = "Hello, World!" };
            var serialized = JsonSerializer.Serialize(response);

            ctx.Response.Headers.Add("Content-Type", "application/json; charset=UTF-8");

            await ctx.Response.Send(serialized);
        }

    }

}
