using System;
using System.Threading.Tasks;
using Benchmarks.Configuration;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;

namespace Benchmarks.Middleware
{
    public class SpanJsonMiddleware
    {
        private static readonly PathString _path = new PathString(Scenarios.GetPath(s => s.SpanJson));
        private const int _bufferSize = 27;

        private readonly RequestDelegate _next;

        public SpanJsonMiddleware(RequestDelegate next)
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
                var result = SpanJson.JsonSerializer.Generic.Utf8.SerializeAsync(msg, httpContext.Response.Body);
                return result.IsCompleted ? Task.CompletedTask : result.AsTask();
            }

            return _next(httpContext);
        }
    }

    public static class SpanJsonMiddlewareExtensions
    {
        public static IApplicationBuilder UseSpanJson(this IApplicationBuilder builder)
        {
            return builder.UseMiddleware<SpanJsonMiddleware>();
        }
    }
}