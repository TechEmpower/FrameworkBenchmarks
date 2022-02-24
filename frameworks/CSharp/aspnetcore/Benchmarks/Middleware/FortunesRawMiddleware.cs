// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Text.Encodings.Web;
using Benchmarks.Configuration;
using Benchmarks.Data;

namespace Benchmarks.Middleware;

public class FortunesRawMiddleware
{
    private static readonly PathString _path = new(Scenarios.GetPath(s => s.DbFortunesRaw));

    private readonly RequestDelegate _next;
    private readonly HtmlEncoder _htmlEncoder;

    public FortunesRawMiddleware(RequestDelegate next, HtmlEncoder htmlEncoder)
    {
        _next = next;
        _htmlEncoder = htmlEncoder;
    }

    public async Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.StartsWithSegments(_path, StringComparison.Ordinal))
        {
            var db = httpContext.RequestServices.GetService<RawDb>();
            var rows = await db.LoadFortunesRows();

            await MiddlewareHelpers.RenderFortunesHtml(rows, httpContext, _htmlEncoder);

            return;
        }

        await _next(httpContext);
    }
}

public static class FortunesRawMiddlewareExtensions
{
    public static IApplicationBuilder UseFortunesRaw(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<FortunesRawMiddleware>();
    }
}
