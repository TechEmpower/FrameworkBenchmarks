using System.Text;
using System.Text.Json.Serialization;
using TouchSocket.Core;
using TouchSocket.Http;
using TouchSocket.Rpc;
using TouchSocket.Sockets;
using TouchSocket.WebApi;
using TouchSocket.WebApi.Swagger;
using HttpContent = TouchSocket.Http.HttpContent;

namespace TouchSocketWebApi;

public class Program
{
    public static void Main(string[] args)
    {
        var builder = Host.CreateApplicationBuilder(args);

        builder.Services.AddServiceHostedService<IHttpService, HttpService>(config =>
        {
            config.SetListenIPHosts(8080)
            .SetNoDelay(true)
            .SetMaxCount(1000000)
            .SetBacklog(1000)
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
               a.UseTcpSessionCheckClear();

               a.UseWebApi()
               .ConfigureConverter(converter =>
               {
                   converter.Clear();
                   converter.AddSystemTextJsonSerializerFormatter(options =>
                   {
                       options.TypeInfoResolverChain.Insert(0, AppJsonSerializerContext.Default);
                   });
               });

#if DEBUG
               a.UseSwagger()
               .UseLaunchBrowser();
#endif

               a.UseDefaultHttpServicePlugin();
           });
        });

        var host = builder.Build();
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
       var response= callContext.HttpContext.Response;
        response.SetStatus(200, "success");
        response.Content= m_contentPlaintext;
        await response.AnswerAsync().ConfigureAwait(false);
    }

    [Router("/json")]
    [WebApi(Method = HttpMethodType.Get)]
    public MyJson Json()
    {
        return MyJson;
    }
}

[JsonSerializable(typeof(MyJson))]//实际类型1
internal partial class AppJsonSerializerContext : JsonSerializerContext
{

}

public class MyJson
{
    public string? Message { get; set; }
}