using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;
using appMpowerAot;

public unsafe class PlaintextMiddleware
{
    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new KeyValuePair<string, StringValues>("Content-Type", new StringValues("text/plain"));

    private static readonly byte[] HelloWorldPayload = Encoding.UTF8.GetBytes(new string(NativeMethods.HelloWorld()));
    //TEST
    //private static readonly byte[] HelloWorldPayload = Encoding.UTF8.GetBytes(DotnetMethods.HelloWorld());
    private readonly RequestDelegate _nextStage;

    public PlaintextMiddleware(RequestDelegate nextStage)
    {
        _nextStage = nextStage;
    }

    public Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.Value.StartsWith("/p"))
        {
            var payloadLength = HelloWorldPayload.Length;
            var response = httpContext.Response; 
            response.Headers.Add(_headerServer);
            response.Headers.Add(_headerContentType);
            response.Headers.Add(
                new KeyValuePair<string, StringValues>("Content-Length", payloadLength.ToString()));

            return response.Body.WriteAsync(HelloWorldPayload, 0, payloadLength);
        }

        return _nextStage(httpContext);
    }
}

public static class PlaintextMiddlewareExtensions
{
    public static IApplicationBuilder UsePlainText(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<PlaintextMiddleware>();
    }
}
