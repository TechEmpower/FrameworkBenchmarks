using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

namespace appMpower; 

public class PlaintextMiddleware
{
    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new("Content-Type", new StringValues("text/plain"));
    private static readonly byte[] _helloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");

    public static async Task Invoke(HttpContext httpContext)
    {
        var payloadLength = _helloWorldPayload.Length;
        var response = httpContext.Response; 
        response.Headers.Add(_headerServer);
        response.Headers.Add(_headerContentType);
        response.Headers.Add(
            new KeyValuePair<string, StringValues>("Content-Length", payloadLength.ToString()));

        await response.Body.WriteAsync(_helloWorldPayload);
    }
}