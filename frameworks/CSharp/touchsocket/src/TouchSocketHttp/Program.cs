using System.Text;
using TouchSocket.Core;
using TouchSocket.Http;
using TouchSocket.Sockets;
using HttpContent = TouchSocket.Http.HttpContent;

namespace TouchSocketHttp;

public class Program
{
    static async Task Main(string[] args)
    {
        int port = 8080;

        var service = new MyHttpService();

        await service.SetupAsync(new TouchSocketConfig()
             .SetListenIPHosts(port)
             .SetNoDelay(true)
             .SetMaxCount(1000000)
             .SetBacklog(1000)
             .ConfigureContainer(a =>
             {
                 a.AddConsoleLogger();
             }));

        await service.StartAsync();
        service.Logger.Info($"server is started,port:{port}");
        while (true)
        {
            Console.ReadLine();
        }
    }
}

sealed class MyHttpService : HttpService<MyHttpSessionClient>
{
    protected override MyHttpSessionClient NewClient()
    {
        return new MyHttpSessionClient();
    }
}

sealed class MyHttpSessionClient : HttpSessionClient
{
    HttpContent m_contentPlaintext = StringHttpContent.FromText("Hello, World!");
    HttpContent m_contentJson = StringHttpContent.FromJson("{\"Message\":\"Hello, World!\"}");

    protected override async Task OnReceivedHttpRequest(HttpContext httpContext)
    {
        var request = httpContext.Request;
        var response = httpContext.Response;

        switch (request.RelativeURL)
        {
            case "/plaintext":
                {
                    response.SetStatus(200, "success");
                    response.SetContent(m_contentPlaintext);
                    await response.AnswerAsync().ConfigureAwait(false);
                }
                break;
            case "/json":
                {
                    response.SetStatus(200, "success");
                    response.SetContent(m_contentJson);
                    await response.AnswerAsync().ConfigureAwait(false);
                }
                break;
            default:
                response.SetStatus(404, "not find");
                await response.AnswerAsync().ConfigureAwait(false);
                break;
        }
    }
}

