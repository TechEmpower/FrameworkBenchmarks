using System.Text;
using TouchSocket.Core;
using TouchSocket.Http;
using TouchSocket.Sockets;
using static System.Net.Mime.MediaTypeNames;
using HttpContent = TouchSocket.Http.HttpContent;

namespace TouchSocketHttp;

public class Program
{
    static async Task Main(string[] args)
    {
        int port = 8080;

        Console.WriteLine(DateHelper.DateString);
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
    private readonly HttpContent m_contentPlaintext = new StringHttpContent("Hello, World!", Encoding.UTF8, $"text/plain");
    private readonly HttpContent m_contentJson = new StringHttpContent("{\"message\":\"Hello, World!\"}", Encoding.UTF8, $"application/json");

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
                    response.Headers.Add(HttpHeaders.Server, "T");
                    response.Headers.Add(HttpHeaders.Date, DateHelper.DateString);
                    response.Content = m_contentPlaintext;
                    await response.AnswerAsync().ConfigureAwait(false);
                }
                break;
            case "/json":
                {
                    response.StatusCode = 200;
                    response.StatusMessage = "success";
                    response.Headers.Add(HttpHeaders.Server, "T");
                    response.Headers.Add(HttpHeaders.Date, DateHelper.DateString);
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

static class DateHelper
{
    static Timer m_timer;
    static DateHelper()
    {
        m_timer = new Timer((state) =>
            {
                DateString = DateTime.UtcNow.ToGMTString();
            }, null, 0, 1000);
    }

    public static string DateString { get; private set; }
}

