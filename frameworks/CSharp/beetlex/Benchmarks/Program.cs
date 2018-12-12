using BeetleX.FastHttpApi;
using Microsoft.Extensions.Hosting;
using System.Threading.Tasks;
using Microsoft.Extensions.DependencyInjection;
using System;
using System.Threading;
<<<<<<< HEAD
=======
using System.Text;

>>>>>>> master
namespace Benchmarks
{
    [BeetleX.FastHttpApi.Controller]
    class Program
    {
<<<<<<< HEAD
        public static void Main(string[] args)
        {
=======
        private static readonly byte[] _helloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");

        private static StringBytes plaintextResult;

        private static JsonMessage jsonMessage = new JsonMessage { message = "Hello, World!" };

        public static void Main(string[] args)
        {
            plaintextResult = new StringBytes(_helloWorldPayload);
>>>>>>> master
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
<<<<<<< HEAD
            return new TextResult("Hello, World!");
=======
            return plaintextResult;
>>>>>>> master
        }

        public object json(IHttpContext context)
        {
            context.Response.Header[HeaderTypeFactory.DATE] = DateTime.Now.ToUniversalTime().ToString("r");
<<<<<<< HEAD
            return new JsonResult(new JsonMessage { message = "Hello, World!" });
=======
            return new JsonResult(jsonMessage);
>>>>>>> master
        }

        public class JsonMessage
        {
            public string message { get; set; }
        }
    }

<<<<<<< HEAD
=======


>>>>>>> master
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
