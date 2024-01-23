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
using BeetleX.EventArgs;

namespace Benchmarks
{
    [Controller]
    class Program : IController
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

        public async Task<object> queries(int queries, IHttpContext context)
        {
            queries = queries < 1 ? 1 : queries > 500 ? 500 : queries;
            var result = await GetDB(context).LoadMultipleQueriesRows(queries);
            return new SpanJsonResult(result);
        }

        public RawDb GetDB(IHttpContext context)
        {
            return (RawDb)context.Session["DB"];
        }

        public async Task<object> db(IHttpContext context)
        {
            var result = await GetDB(context).LoadSingleQueryRow();
            return new SpanJsonResult(result);
        }

        public async Task<object> fortunes(IHttpContext context)
        {
            var data = await GetDB(context).LoadFortunesRows();
            return new FortuneView(data);
        }


        public async Task<object> updates(int queries, IHttpContext context)
        {
            queries = queries < 1 ? 1 : queries > 500 ? 500 : queries;
            var result = await GetDB(context).LoadMultipleUpdatesRows(queries);
            return new SpanJsonResult(result);
        }


        [NotAction]
        public void Init(HttpApiServer server, string path)
        {

        }
    }

    public class BeetleXHttpServer : IHostedService
    {

        private static readonly byte[] _helloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");

        public static StringBytes plaintextResult;

        private HttpApiServer mApiServer;

        private System.Threading.Tasks.TaskCompletionSource<object> mComplete = new TaskCompletionSource<object>();

        public async virtual Task StartAsync(CancellationToken cancellationToken)
        {


            plaintextResult = new StringBytes(_helloWorldPayload);
            mApiServer = new HttpApiServer();
            mApiServer.Options.Port = 8080;
            mApiServer.Options.BufferPoolMaxMemory = 500;
            mApiServer.Options.MaxConnections = 100000;
            mApiServer.Options.Statistical = false;
            mApiServer.Options.LogLevel = BeetleX.EventArgs.LogType.Error;
            mApiServer.Options.LogToConsole = true;
            mApiServer.Register(typeof(Program).Assembly);
            HeaderTypeFactory.SERVAR_HEADER_BYTES = Encoding.ASCII.GetBytes("Server: TFB\r\n");
            mApiServer.HttpConnected += (o, e) =>
            {
                e.Session["DB"] = new RawDb(new ConcurrentRandom(), Npgsql.NpgsqlFactory.Instance);
            };
            mApiServer.Started += (o, e) =>
            {
                mComplete.TrySetResult(new object());
            };
            mApiServer.Open();
            RawDb._connectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Delay Us=500;Write Coalescing Buffer Threshold Bytes=1000";
            //RawDb._connectionString = "Server=192.168.2.19;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=256;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3";
            await mComplete.Task;
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
