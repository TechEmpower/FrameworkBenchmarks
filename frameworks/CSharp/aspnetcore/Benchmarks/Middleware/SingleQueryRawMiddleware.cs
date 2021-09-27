// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Text.Json;
using System.Threading.Tasks;
using Benchmarks.Configuration;
using Benchmarks.Data;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks.Middleware
{
    public class SingleQueryRawMiddleware
    {
        private static readonly PathString _path = new PathString(Scenarios.GetPath(s => s.DbSingleQueryRaw));
        private static readonly JsonSerializerOptions _serializerOptions = new() { PropertyNamingPolicy = JsonNamingPolicy.CamelCase };

        private readonly RequestDelegate _next;

        public SingleQueryRawMiddleware(RequestDelegate next)
        {
            _next = next;
        }

        public async Task Invoke(HttpContext httpContext)
        {
            if (httpContext.Request.Path.StartsWithSegments(_path, StringComparison.Ordinal))
            {
                var db = httpContext.RequestServices.GetService<RawDb>();
                var row = await db.LoadSingleQueryRow();

                var result = JsonSerializer.Serialize(row, _serializerOptions);

                httpContext.Response.StatusCode = StatusCodes.Status200OK;
                httpContext.Response.ContentType = "application/json";
                httpContext.Response.ContentLength = result.Length;

                await httpContext.Response.WriteAsync(result);

                return;
            }

            await _next(httpContext);
        }
    }

    public static class SingleQueryRawMiddlewareExtensions
    {
        public static IApplicationBuilder UseSingleQueryRaw(this IApplicationBuilder builder)
        {
            return builder.UseMiddleware<SingleQueryRawMiddleware>();
        }
    }
}
