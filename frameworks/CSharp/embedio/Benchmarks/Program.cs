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

        public static async Task<int> Main(string[] args)
        {
            Logger.UnregisterLogger<ConsoleLogger>();

            using var server = new WebServer(o => o
                  .WithUrlPrefix("http://+:8080/")
                  .WithMode(HttpListenerMode.EmbedIO))
                .PreferNoCompressionFor("text/*")
                .WithAction("/plaintext", HttpVerbs.Get, async (ctx) =>
                {
                    var bytes = Encoding.UTF8.GetBytes("Hello, World!");

                    ctx.Response.ContentType = "text/plain";
                    ctx.Response.ContentEncoding = Encoding.UTF8;
                    ctx.Response.ContentLength64 = bytes.Length;

                    await ctx.Response.OutputStream.WriteAsync(bytes, 0, bytes.Length);
                })
                .WithAction("/json", HttpVerbs.Get, async (ctx) =>
                {
                    var data = new JsonResult() { Message = "Hello, World!" };

                    var serialized = Swan.Formatters.Json.Serialize(data);

                    var bytes = Encoding.UTF8.GetBytes(serialized);

                    ctx.Response.ContentType = "application/json";
                    ctx.Response.ContentEncoding = Encoding.UTF8;
                    ctx.Response.ContentLength64 = bytes.Length;

                    await ctx.Response.OutputStream.WriteAsync(bytes, 0, bytes.Length);
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
