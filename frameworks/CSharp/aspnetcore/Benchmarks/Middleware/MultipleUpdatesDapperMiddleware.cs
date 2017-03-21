// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System;
using System.Threading.Tasks;
using Benchmarks.Configuration;
using Benchmarks.Data;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace Benchmarks.Middleware
{
    public class MultipleUpdatesDapperMiddleware
    {
        private static readonly PathString _path = new PathString(Scenarios.GetPath(s => s.DbMultiUpdateDapper));
        private static readonly JsonSerializerSettings _jsonSettings = new JsonSerializerSettings
        {
            ContractResolver = new CamelCasePropertyNamesContractResolver()
        };

        private readonly RequestDelegate _next;
        private readonly DapperDb _db;

        public MultipleUpdatesDapperMiddleware(RequestDelegate next, DapperDb db)
        {
            _next = next;
            _db = db;
        }

        public async Task Invoke(HttpContext httpContext)
        {
            if (httpContext.Request.Path.StartsWithSegments(_path, StringComparison.Ordinal))
            {
                var count = MiddlewareHelpers.GetMultipleQueriesQueryCount(httpContext);
                var rows = await _db.LoadMultipleUpdatesRows(count);

                var result = JsonConvert.SerializeObject(rows, _jsonSettings);

                httpContext.Response.StatusCode = StatusCodes.Status200OK;
                httpContext.Response.ContentType = "application/json";
                httpContext.Response.ContentLength = result.Length;

                await httpContext.Response.WriteAsync(result);

                return;
            }

            await _next(httpContext);
        }
    }

    public static class MultipleUpdatesDapperMiddlewareExtensions
    {
        public static IApplicationBuilder UseMultipleUpdatesDapper(this IApplicationBuilder builder)
        {
            return builder.UseMiddleware<MultipleUpdatesDapperMiddleware>();
        }
    }
}