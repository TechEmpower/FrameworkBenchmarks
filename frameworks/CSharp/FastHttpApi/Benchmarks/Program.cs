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
        public static void Main(string[] args)
        {
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
            context.Response.Header[HeaderTypeFactory.DATE] = DateTime.Now.ToUniversalTime().ToString("r");
            return new TextResult("Hello, World!");
        }
        public object json(IHttpContext context)
        {
            context.Response.Header[HeaderTypeFactory.DATE] = DateTime.Now.ToUniversalTime().ToString("r");
            return new JsonResult(new JsonMessage { message = "Hello, World!" });
        }

        public class JsonMessage
        {
            public string message { get; set; }
        }


    }


    public class BeetleXHttpServer : BackgroundService
    {
        private HttpApiServer mApiServer;

        protected override async Task ExecuteAsync(CancellationToken stoppingToken)
        {
            mApiServer = new HttpApiServer();
            mApiServer.Register(typeof(Program).Assembly);
            mApiServer.ServerConfig.Port = 8080;
            mApiServer.ServerConfig.UrlIgnoreCase = false;
            mApiServer.ServerConfig.LogLevel = BeetleX.EventArgs.LogType.Warring;
            mApiServer.ServerConfig.LogToConsole = true;
            mApiServer.Open();
            Console.WriteLine("BeetleX FastHttpApi server");
            Console.WriteLine($"ServerGC:{System.Runtime.GCSettings.IsServerGC}");
            Console.Write(mApiServer.BaseServer);
            while (!stoppingToken.IsCancellationRequested)
            {
                Console.WriteLine("HttpApiServer is doing background work.");
                await Task.Delay(TimeSpan.FromSeconds(5), stoppingToken);
            }
        }
    }

}
