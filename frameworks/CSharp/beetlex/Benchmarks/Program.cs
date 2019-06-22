using BeetleX.FastHttpApi;
using Microsoft.Extensions.Hosting;
using System.Threading.Tasks;
using Microsoft.Extensions.DependencyInjection;
using System;
using System.Threading;
using System.Text;
using BeetleX.Buffers;
using SpanJson;
using System.Collections.Generic;

namespace Benchmarks
{
    [Controller]
    class Program:IController
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

        public object plaintext()
        {
            return BeetleXHttpServer.plaintextResult;
        }

        public object json()
        {
            return new SpanJsonResult(new JsonMessage { message = "Hello, World!" });
        }

        public async Task<object> queries(int queries)
        {
            queries = queries < 1 ? 1 : queries > 500 ? 500 : queries;
            var result = await mPgsql.LoadMultipleQueriesRows(queries);
            return new SpanJsonResult(result);
        }

        public async Task<object> db()
        {
            var result = await mPgsql.LoadSingleQueryRow();
            return new SpanJsonResult(result);
        }

        public async Task<object> fortunes()
        {
            var data = await mPgsql.LoadFortunesRows();
            return new FortuneView(data);
        }


        private RawDb mPgsql;

        [NotAction]
        public void Init(HttpApiServer server, string path)
        {
            mPgsql = new RawDb(new ConcurrentRandom(), Npgsql.NpgsqlFactory.Instance);
        }
    }

    public class BeetleXHttpServer : IHostedService
    {

        private static readonly byte[] _helloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");

        public static StringBytes plaintextResult;

        private HttpApiServer mApiServer;

        public virtual Task StartAsync(CancellationToken cancellationToken)
        {
            plaintextResult = new StringBytes(_helloWorldPayload);
            mApiServer = new HttpApiServer();
            mApiServer.Options.Port = 8080;
            mApiServer.Options.BufferPoolMaxMemory = 500;
            mApiServer.Options.MaxConnections = 100000;
            mApiServer.Options.Statistical = false;
            mApiServer.Options.UrlIgnoreCase = false;
            mApiServer.Options.LogLevel = BeetleX.EventArgs.LogType.Error;
            mApiServer.Options.LogToConsole = true;
            mApiServer.Options.PrivateBufferPool = true;
            mApiServer.Register(typeof(Program).Assembly);
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
