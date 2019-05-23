using BeetleX.FastHttpApi;
using Microsoft.Extensions.Hosting;
using System.Threading.Tasks;
using Microsoft.Extensions.DependencyInjection;
using System;
using System.Threading;
using System.Text;
using BeetleX.Buffers;
using SpanJson;

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
            return plaintextResult;
        }

        public object json(IHttpContext context)
        {
            return new SpanJsonResult(new JsonMessage { message = "Hello, World!" });
        }
        public class JsonMessage
        {
            public string message { get; set; }
        }
    }

    public class SpanJsonResult : ResultBase
    {
        public SpanJsonResult(object data)
        {
            Data = data;
        }

        public object Data { get; set; }

        public override string ContentType => "application/json";

        public override bool HasBody => true;

        public override void Write(PipeStream stream, HttpResponse response)
        {
            using (stream.LockFree())
            {
                var task = JsonSerializer.NonGeneric.Utf8.SerializeAsync(Data, stream).AsTask();
                task.Wait();
            }
        }
    }

    public class BeetleXHttpServer : IHostedService
    {
        private HttpApiServer mApiServer;

        public virtual Task StartAsync(CancellationToken cancellationToken)
        {
            mApiServer = new HttpApiServer();
            mApiServer.Register(typeof(Program).Assembly);
            mApiServer.Options.Port = 8080;
            mApiServer.Options.BufferPoolMaxMemory = 500;
            mApiServer.Options.MaxConnections = 100000;
            mApiServer.Options.Statistical = false;
            mApiServer.Options.UrlIgnoreCase = false;
            mApiServer.Options.LogLevel = BeetleX.EventArgs.LogType.Off;
            mApiServer.Options.LogToConsole = true;
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
