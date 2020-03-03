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

        public async virtual Task StartAsync(CancellationToken cancellationToken)
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
            mApiServer.HttpConnected += (o, e) => {
                e.Session["DB"] = new RawDb(new ConcurrentRandom(), Npgsql.NpgsqlFactory.Instance);
            };
            mApiServer.Open();
            System.Net.Http.HttpClient client = new System.Net.Http.HttpClient();
            var response = await client.GetAsync("http://localhost:8080/json");
            mApiServer.BaseServer.Log(LogType.Info, null, $"Get josn {response.StatusCode}");
            response = await client.GetAsync("http://localhost:8080/plaintext");
            mApiServer.BaseServer.Log(LogType.Info, null, $"Get plaintext {response.StatusCode}");
          
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
