using System.Buffers;
using System.IO.Pipelines;
using System.Text;
using System.Text.Json.Serialization;
using TouchSocket.Core;
using TouchSocket.Http;
using TouchSocket.Rpc;
using TouchSocket.WebApi;
using HttpContent = TouchSocket.Http.HttpContent;

namespace TouchSocketWebApi;

public class Program
{
    public static void Main(string[] args)
    {
        HostApplicationBuilder builder = Host.CreateApplicationBuilder(args);

        builder.Services.AddServiceHostedService<IHttpService, HttpService>(config =>
        {
            config.SetListenIPHosts(8080)
            .SetMaxCount(1000000)
            .SetTransportOption(options =>
            {
                options.BufferOnDemand = false;

                options.ReceivePipeOptions = new PipeOptions(
                pool: MemoryPool<byte>.Shared,
                readerScheduler: PipeScheduler.Inline,
                writerScheduler: PipeScheduler.Inline,
                pauseWriterThreshold: 1024 * 1024,
                resumeWriterThreshold: 1024 * 512,
                minimumSegmentSize: 4096,
                useSynchronizationContext: false);

                options.SendPipeOptions = new PipeOptions(
              pool: MemoryPool<byte>.Shared,
              readerScheduler: PipeScheduler.Inline,
              writerScheduler: PipeScheduler.Inline,
              pauseWriterThreshold: 64 * 1024,
              resumeWriterThreshold: 32 * 1024,
              minimumSegmentSize: 4096,
              useSynchronizationContext: false);
            })
           .ConfigureContainer(a =>
           {
               a.AddConsoleLogger();
               a.AddRpcStore(store =>
               {
                   store.RegisterServer<ApiServer>();
               });
           })
           .ConfigurePlugins(a =>
           {
               a.UseWebApi(options =>
               {
                   options.ConfigureConverter(converter =>
                   {
                       converter.Clear();
                       converter.AddSystemTextJsonSerializerFormatter(jsonOptions =>
                       {
                           jsonOptions.TypeInfoResolverChain.Insert(0, AppJsonSerializerContext.Default);
                       });
                   });
               });

               a.UseDefaultHttpServicePlugin();
           });
        });

        IHost host = builder.Build();
        host.Run();
    }
}

public partial class ApiServer : SingletonRpcServer
{
    private readonly HttpContent m_contentPlaintext = new StringHttpContent("Hello, World!", Encoding.UTF8, $"text/plain");

    public static MyJson MyJson { get; set; } = new MyJson() { Message = "Hello, World!" };

    [Router("/plaintext")]
    [WebApi(Method = HttpMethodType.Get)]
    public async Task Plaintext(IWebApiCallContext callContext)
    {
        HttpResponse response = callContext.HttpContext.Response;
        response.SetStatus(200, "ok");
        response.Content = m_contentPlaintext;
        await response.AnswerAsync().ConfigureAwait(false);
    }

    [Router("/json")]
    [WebApi(Method = HttpMethodType.Get)]
    public MyJson Json()
    {
        return MyJson;
    }
}

[JsonSerializable(typeof(MyJson))]
internal partial class AppJsonSerializerContext : JsonSerializerContext
{

}

public class MyJson
{
    public string? Message { get; set; }
}