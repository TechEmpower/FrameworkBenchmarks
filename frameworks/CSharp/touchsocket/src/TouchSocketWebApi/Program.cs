using TouchSocket.Core;
using TouchSocket.Http;
using TouchSocket.Rpc;
using TouchSocket.Sockets;
using TouchSocket.WebApi;
using TouchSocket.WebApi.Swagger;

namespace TouchSocketWebApi
{
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

                   a.UseWebApi();

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
        public object Json()
        {
            return new { message = "Hello, World!" };
        }
    }
}