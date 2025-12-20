using System.Buffers.Text;
using System.Net;
using System.Text;
using System.Text.Json;
using Sisk.Cadente;

using var host = new HttpHost ( new IPEndPoint ( IPAddress.Any, 8080 ) );
host.Handler = new DefaultHandler();

host.Start ();
Thread.Sleep ( Timeout.Infinite );


class DefaultHandler : HttpHostHandler
{
    public override async Task OnContextCreatedAsync(HttpHost host, HttpHostContext context)
    {
        var request = context.Request;
        var response = context.Response;

        if (request.Path == "/plaintext")
        {
            var contentBytes = Encoding.UTF8.GetBytes("Hello, World!"); 

            await SerializeResponseAsync(response, contentBytes, "text/plain; charset=utf-8");
        }
        else if (request.Path == "/json")
        {
            var contentBytes = JsonSerializer.SerializeToUtf8Bytes(new
            {
                message = "Hello, World!"
            });

            await SerializeResponseAsync(response, contentBytes, "application/json; charset=utf-8");
        }
        else
        {
            response.StatusCode = 404;
        }
    }

    static async ValueTask SerializeResponseAsync(HttpHostContext.HttpResponse response, Memory<byte> content, string contentType)
    { 
        response.Headers.Add(new HttpHeader("Content-Type", contentType));
        response.Headers.Add(new HttpHeader("Content-Length", content.Length.ToString()));

        using var responseStream = await response.GetResponseStreamAsync();
        await responseStream.WriteAsync(content);
    }
}