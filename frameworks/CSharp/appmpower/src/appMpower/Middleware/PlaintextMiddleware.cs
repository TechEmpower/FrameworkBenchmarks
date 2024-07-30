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
            var response = httpContext.Response; 
            //response.Headers["Server"] = "k";
            response.Headers.Add(_headerServer);
            response.StatusCode = 200;
            response.ContentType = "text/plain";

            var payloadLength = HelloWorldPayload.Length;
            response.ContentLength = payloadLength;
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
