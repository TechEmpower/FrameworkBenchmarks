// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System;
using System.Text.Encodings.Web;
using System.Threading.Tasks;
using Benchmarks.Configuration;
using Benchmarks.Data;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;

namespace Benchmarks.Middleware
{
    public class FortunesRawMiddleware
    {
        private static readonly PathString _path = new PathString(Scenarios.GetPath(s => s.DbFortunesRaw));

        private readonly RequestDelegate _next;
        private readonly RawDb _db;
        private readonly HtmlEncoder _htmlEncoder;

        public FortunesRawMiddleware(RequestDelegate next, RawDb db, HtmlEncoder htmlEncoder)
        {
            _next = next;
            _db = db;
            _htmlEncoder = htmlEncoder;
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
    
    public static class FortunesRawMiddlewareExtensions
    {
        public static IApplicationBuilder UseFortunesRaw(this IApplicationBuilder builder)
        {
            return builder.UseMiddleware<FortunesRawMiddleware>();
        }
    }
}
