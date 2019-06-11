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
    [Controller]
    class Program
    {
        public static void Main(string[] args)
        {
            var builder = new HostBuilder()
                .ConfigureServices((hostContext, services) =>
                {
                    services.AddHostedService<BeetleXHttpServer>();
                });
            builder.Build().Run();
        }
    }


    public class BeetleXHttpServer : IHostedService
    {

        private static readonly byte[] _helloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");

        public static StringBytes plaintextResult;

        private HttpApiServer mApiServer;

        public void OnRequesting(object sender, EventHttpRequestArgs e)
        {
            if (e.Request.BaseUrl == "/plaintext")
            {
                e.Response.Result(plaintextResult);
            }
            else if (e.Request.BaseUrl == "/json")
            {
                var json = new SpanJsonResult(new JsonMessage { message = "Hello, World!" });
                e.Response.Result(json);
            }
            else
            {
                e.Response.Result(new NotFoundResult("url not found!"));
            }
            e.Cancel = true;
        }

        public virtual Task StartAsync(CancellationToken cancellationToken)
        {
            plaintextResult = new StringBytes(_helloWorldPayload);
            mApiServer = new HttpApiServer();
            mApiServer.Register(typeof(Program).Assembly);
            mApiServer.Options.Port = 8080;
            mApiServer.Options.BufferPoolMaxMemory = 500;
            mApiServer.Options.MaxConnections = 100000;
            mApiServer.Options.Statistical = false;
            mApiServer.Options.UrlIgnoreCase = false;
            mApiServer.Options.LogLevel = BeetleX.EventArgs.LogType.Off;
            mApiServer.Options.LogToConsole = true;
            mApiServer.Options.PrivateBufferPool = true;
            mApiServer.HttpRequesting += OnRequesting;
            mApiServer.Open();
            return Task.CompletedTask;
        }

        public virtual Task StopAsync(CancellationToken cancellationToken)
        {
            mApiServer.BaseServer.Dispose();
            return Task.CompletedTask;
        }
    }
    public class JsonMessage
    {
        public string message { get; set; }
    }

    public class SpanJsonResult : ResultBase
    {
        public SpanJsonResult(object data)
        {
            Data = data;
        }

        public object Data { get; set; }

        public override IHeaderItem ContentType => ContentTypes.JSON;

        public override bool HasBody => true;

        public override void Write(PipeStream stream, HttpResponse response)
        {
            JsonSerializer.NonGeneric.Utf8.SerializeAsync(Data, stream);
        }
    }
}
