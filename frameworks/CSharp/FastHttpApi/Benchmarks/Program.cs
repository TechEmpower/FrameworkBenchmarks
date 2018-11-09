using BeetleX.FastHttpApi;
using Microsoft.Extensions.Hosting;
using System.Threading.Tasks;
using Microsoft.Extensions.DependencyInjection;
using System;
using System.Threading;


namespace Benchmarks
{

    [BeetleX.FastHttpApi.Controller]
    class Program
    {
        private static string mDate;
        public static void Main(string[] args)
        {
            mDate = DateTime.Now.ToUniversalTime().ToString("r");
            var builder = new HostBuilder()
                .ConfigureServices((hostContext, services) =>
                {
                    services.AddHostedService<BeetleXHttpServer>();

                });

            var result = builder.RunConsoleAsync();
            result.Wait();

        }

        public object plaintext(IHttpContext context)
        {
            context.Response.Header[HeaderTypeFactory.DATE] = mDate;
            return new TextResult("Hello, World!");
        }
        public object json(IHttpContext context)
        {
            context.Response.Header[HeaderTypeFactory.DATE] = mDate;
            return new JsonResult(new JsonMessage { message = "Hello, World!" });
        }

        public class JsonMessage
        {
            public string message { get; set; }
        }


    }

    public class BeetleXHttpServer : IHostedService
    {
        private HttpApiServer mApiServer;

        public virtual Task StartAsync(CancellationToken cancellationToken)
        {
            mApiServer = new HttpApiServer();
            mApiServer.Register(typeof(Program).Assembly);
            mApiServer.ServerConfig.Port = 8080;
            mApiServer.ServerConfig.MaxConnections = 100000;
            mApiServer.ServerConfig.UrlIgnoreCase = false;
            mApiServer.ServerConfig.LogLevel = BeetleX.EventArgs.LogType.Warring;
            mApiServer.ServerConfig.LogToConsole = true;
            mApiServer.Open();
            Console.WriteLine("BeetleX FastHttpApi server");
            Console.WriteLine($"ServerGC:{System.Runtime.GCSettings.IsServerGC}");
            Console.Write(mApiServer.BaseServer);
            return Task.CompletedTask;
        }

        public virtual async Task StopAsync(CancellationToken cancellationToken)
        {
            return;

        }
    }
}
