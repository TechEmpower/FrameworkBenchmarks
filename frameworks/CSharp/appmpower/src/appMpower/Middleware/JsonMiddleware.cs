using System;
using System.Text.Json;
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

    public Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.StartsWithSegments("/json", StringComparison.Ordinal))
        {
            httpContext.Response.StatusCode = 200;
            httpContext.Response.ContentType = "application/json";
            httpContext.Response.ContentLength = BufferSize;

            var jsonMessage = new JsonMessage
            {
                message = "Hello, World!"
            };

            return JsonSerializer.SerializeAsync(httpContext.Response.Body, jsonMessage);
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

public class JsonMessage
{
    public string message { get; set; }
}