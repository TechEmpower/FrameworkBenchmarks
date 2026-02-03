using System.Buffers;
using System.IO.Pipelines;
using System.Text;
using TouchSocket.Core;
using TouchSocket.Http;
using HttpContent = TouchSocket.Http.HttpContent;

namespace TouchSocketHttp;

public class Program
{
    private static async Task Main(string[] args)
    {
        int port = 8080;
        MyHttpService service = new MyHttpService();

        await service.SetupAsync(new TouchSocketConfig()
             .SetListenIPHosts(port)
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
             }));

        await service.StartAsync();
        service.Logger.Info($"server is started,port:{port}");
        while (true)
        {
            Console.ReadLine();
        }
    }
}

internal sealed class MyHttpService : HttpService<MyHttpSessionClient>
{
    protected override MyHttpSessionClient NewClient()
    {
        return new MyHttpSessionClient();
    }
}

internal sealed class MyHttpSessionClient : HttpSessionClient
{
    private readonly HttpContent m_contentPlaintext = new StringHttpContent("Hello, World!", Encoding.UTF8, "text/plain");
    private readonly HttpContent m_contentJson = new StringHttpContent("{\"message\":\"Hello, World!\"}", Encoding.UTF8, "application/json");

    protected override async Task OnReceivedHttpRequest(HttpContext httpContext)
    {
        HttpRequest request = httpContext.Request;
        HttpResponse response = httpContext.Response;

        switch (request.RelativeURL)
        {
            case "/plaintext":
                {
                    response.StatusCode = 200;
                    response.StatusMessage = "ok";
                    response.Headers.Add(HttpHeaders.Server, "T");
                    response.Headers.Add(HttpHeaders.Date, HttpExtensions.CurrentHttpDate);
                    response.Content = m_contentPlaintext;
                    await response.AnswerAsync().ConfigureAwait(false);
                }
                break;
            case "/json":
                {
                    response.StatusCode = 200;
                    response.StatusMessage = "ok";
                    response.Headers.Add(HttpHeaders.Server, "T");
                    response.Headers.Add(HttpHeaders.Date, HttpExtensions.CurrentHttpDate);
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