using System.Text.Json.Serialization;
using TouchSocket.Core;
using TouchSocket.Http;
using TouchSocket.Rpc;
using TouchSocket.Sockets;
using TouchSocket.WebApi;
using TouchSocket.WebApi.Swagger;

namespace TouchSocketWebApi;

public class Program
{
    public static void Main(string[] args)
    {
        var builder = Host.CreateApplicationBuilder(args);

        builder.Services.AddServiceHostedService<IHttpService, HttpService>(config =>
        {
            config.SetListenIPHosts(8080)
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
               a.UseCheckClear();

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

public partial class ApiServer : RpcServer
{
    [Router("/plaintext")]
    [WebApi(Method = HttpMethodType.Get)]
    public string Plaintext()
    {
        return "Hello, World!";
    }

    [Router("/json")]
    [WebApi(Method = HttpMethodType.Get)]
    public MyJson Json()
    {
        return new MyJson() { Message = "Hello, World!" };
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