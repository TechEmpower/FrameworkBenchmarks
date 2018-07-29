// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading.Tasks;
using Benchmarks.Data;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks.Controllers
{
    [Route("mvc/updates")]
    public class MultipleUpdatesController : Controller
    {
        [HttpGet("raw")]
        [Produces("application/json")]
        public Task<WorldRaw[]> Raw(int queries = 1)
        {
            queries = queries < 1 ? 1 : queries > 500 ? 500 : queries;
            var db = HttpContext.RequestServices.GetRequiredService<RawDb>();
            return db.LoadMultipleUpdatesRows(queries);
        }

        [HttpGet("ef")]
        [Produces("application/json")]
        public Task<World[]> Ef(int queries = 1)
        {
            queries = queries < 1 ? 1 : queries > 500 ? 500 : queries;
            var db = HttpContext.RequestServices.GetRequiredService<EfDb>();
            return db.LoadMultipleUpdatesRows(queries);
        }
    }
}
