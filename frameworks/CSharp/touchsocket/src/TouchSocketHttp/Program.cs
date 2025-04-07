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
    private HttpContent m_contentPlaintext = StringHttpContent.FromText("Hello, World!");
    private HttpContent m_contentJson = StringHttpContent.FromJson("{\"Message\":\"Hello, World!\"}");

    protected override async Task OnReceivedHttpRequest(HttpContext httpContext)
    {
        var request = httpContext.Request;
        var response = httpContext.Response;

        switch (request.RelativeURL)
        {
            case "/plaintext":
                {
                    response.StatusCode = 200;
                    response.StatusMessage = "success";
                    response.Headers.Add(HttpHeaders.Server, HttpExtensions.HttpHeadersServer);
                    response.Content = m_contentPlaintext;
                    await response.AnswerAsync().ConfigureAwait(false);
                }
                break;
            case "/json":
                {
                    response.StatusCode = 200;
                    response.StatusMessage = "success";
                    response.Headers.Add(HttpHeaders.Server, HttpExtensions.HttpHeadersServer);
                    response.Content = m_contentJson;
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

