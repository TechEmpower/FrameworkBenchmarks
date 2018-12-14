using BeetleX.FastHttpApi;
using Microsoft.Extensions.Hosting;
using System.Threading.Tasks;
using Microsoft.Extensions.DependencyInjection;
using System;
using System.Threading;
using System.Text;
namespace Benchmarks
{
    [BeetleX.FastHttpApi.Controller]
    class Program
    {
        private static readonly byte[] _helloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");

        private static StringBytes plaintextResult;

        public static void Main(string[] args)
        {
            plaintextResult = new StringBytes(_helloWorldPayload);
            var builder = new HostBuilder()
                .ConfigureServices((hostContext, services) =>
                {
                    services.AddHostedService<BeetleXHttpServer>();
                });
            builder.Build().Run();
        }

        public object plaintext(IHttpContext context)
        {
            context.Response.Header[HeaderTypeFactory.DATE] = DateTime.Now.ToUniversalTime().ToString("r");
            return plaintextResult;
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

        public virtual Task StopAsync(CancellationToken cancellationToken)
        {
            mApiServer.BaseServer.Dispose();
            return Task.CompletedTask;
        }
    }
}
