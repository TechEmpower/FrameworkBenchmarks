// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Text.Json;
using Benchmarks.Configuration;

namespace Benchmarks.Middleware;

public sealed class JsonMiddleware
{
    private static readonly PathString _path = new(Scenarios.GetPath(s => s.Json));
    private const int _bufferSize = 27;

    private readonly RequestDelegate _next;

    public JsonMiddleware(RequestDelegate next)
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

            return JsonSerializer.SerializeAsync<JsonMessage>(httpContext.Response.Body, new JsonMessage { message = "Hello, World!" });
        }

        return _next(httpContext);
    }
}

public static class JsonMiddlewareExtensions
{
    public static IApplicationBuilder UseJson(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<JsonMiddleware>();
    }
}

public struct JsonMessage
{
    public string message { get; set; }
}
