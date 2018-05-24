// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Text.Encodings.Web;
using System.Threading.Tasks;
using Benchmarks.Configuration;
using Benchmarks.Data;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks.Middleware
{
    public class FortunesRavenMiddleware
    {
        private static readonly PathString _path = new PathString(Scenarios.GetPath(s => s.DbFortunesRaven));

        private readonly RequestDelegate _next;
        private readonly HtmlEncoder _htmlEncoder;
        private readonly RavenDb _db;

        public FortunesRavenMiddleware(RequestDelegate next, HtmlEncoder htmlEncoder, RavenDb provider)
        {
            _next = next;
            _htmlEncoder = htmlEncoder;
            _db = provider;
        }

        public async Task Invoke(HttpContext httpContext)
        {
            if (httpContext.Request.Path.StartsWithSegments(_path, StringComparison.Ordinal))
            {               
                var rows = await _db.LoadFortunesRows();

                await MiddlewareHelpers.RenderFortunesHtml(rows, httpContext, _htmlEncoder);

                return;
            }

            await _next(httpContext);
        }
    }

    public static class FortunesRavenMiddlewareExtensions
    {
        public static IApplicationBuilder UseFortunesRaven(this IApplicationBuilder builder)
        {
            return builder.UseMiddleware<FortunesRavenMiddleware>();
        }
    }
}
