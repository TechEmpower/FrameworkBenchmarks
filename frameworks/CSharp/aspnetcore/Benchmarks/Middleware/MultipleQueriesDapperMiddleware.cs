// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Text.Json;
using Benchmarks.Configuration;
using Benchmarks.Data;

namespace Benchmarks.Middleware;

public sealed class MultipleQueriesDapperMiddleware
{
    private static readonly PathString _path = new(Scenarios.GetPath(s => s.DbMultiQueryDapper));
    private static readonly JsonSerializerOptions _serializerOptions = new() { PropertyNamingPolicy = JsonNamingPolicy.CamelCase };

    private readonly RequestDelegate _next;

    public MultipleQueriesDapperMiddleware(RequestDelegate next)
    {
        _next = next;
    }

    public async Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.StartsWithSegments(_path, StringComparison.Ordinal))
        {
            var count = MiddlewareHelpers.GetMultipleQueriesQueryCount(httpContext);

            var db = httpContext.RequestServices.GetService<DapperDb>();
            var rows = await db.LoadMultipleQueriesRows(count);

            var result = JsonSerializer.Serialize(rows, _serializerOptions);

            httpContext.Response.StatusCode = StatusCodes.Status200OK;
            httpContext.Response.ContentType = "application/json";
            httpContext.Response.ContentLength = result.Length;

            await httpContext.Response.WriteAsync(result);

            return;
        }

        await _next(httpContext);
    }
}

public static class MultipleQueriesDapperMiddlewareExtensions
{
    public static IApplicationBuilder UseMultipleQueriesDapper(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<MultipleQueriesDapperMiddleware>();
    }
}
