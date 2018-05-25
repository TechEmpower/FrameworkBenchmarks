using Benchmarks.Configuration;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using System;
using System.Threading.Tasks;
using Utf8Json;

namespace Benchmarks.Middleware
{
    public struct JsonMessage
    {
        public string message;
    }

    public class Utf8JsonMiddleware
    {
        private static readonly PathString _path = new PathString(Scenarios.GetPath(s => s.Utf8Json));
        private const int _bufferSize = 27;

        private readonly RequestDelegate _next;

        public Utf8JsonMiddleware(RequestDelegate next)
        {
            _next = next;
        }

        public Task Invoke(HttpContext httpContext)
        {
            if (httpContext.Request.Path.StartsWithSegments(_path, StringComparison.Ordinal))
            {
                httpContext.Response.StatusCode = 200;
                httpContext.Response.ContentType = "application/json";
                httpContext.Response.ContentLength = _bufferSize;

                var msg = new JsonMessage { message = "Hello, World!" };
                JsonSerializer.Serialize(httpContext.Response.Body, msg);

                return Task.CompletedTask;
            }

            return _next(httpContext);
        }
    }

    public static class Utf8JsonMiddlewareExtensions
    {
        public static IApplicationBuilder UseUtf8Json(this IApplicationBuilder builder)
        {
            return builder.UseMiddleware<Utf8JsonMiddleware>();
        }
    }
}