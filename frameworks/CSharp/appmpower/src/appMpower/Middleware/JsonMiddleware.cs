using System;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;

public class JsonMiddleware
{
    private const int BufferSize = 27;
    private readonly RequestDelegate _nextStage;

    public JsonMiddleware(RequestDelegate nextStage)
    {
        _nextStage = nextStage;
    }

    public unsafe Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.StartsWithSegments("/json", StringComparison.Ordinal))
        {
            httpContext.Response.StatusCode = 200;
            httpContext.Response.ContentType = "application/json";

            var jsonMessage = Encoding.UTF8.GetBytes(new string(NativeMethods.JsonMessage()));
            var payloadLength = jsonMessage.Length;
            httpContext.Response.ContentLength = payloadLength; 

            //Console.WriteLine("here we are");
            //Console.WriteLine(jsonMessage);
            //Console.WriteLine("again");

            return httpContext.Response.Body.WriteAsync(jsonMessage, 0, payloadLength);
            //return JsonSerializer.SerializeAsync(httpContext.Response.Body, jsonMessage);
        }

        return _nextStage(httpContext);
    }
}

public static class JsonMiddlewareExtensions
{
    public static IApplicationBuilder UseJson(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<JsonMiddleware>();
    }
}