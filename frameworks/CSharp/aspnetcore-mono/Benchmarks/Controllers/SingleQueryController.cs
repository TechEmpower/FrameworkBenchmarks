// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading.Tasks;
using Benchmarks.Data;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks.Controllers
{
    [Route("mvc/db")]
    public class SingleQueryController : Controller
    {
        [HttpGet("raw")]
        [Produces("application/json")]
        public Task<WorldRaw> Raw()
        {
            var db = HttpContext.RequestServices.GetRequiredService<RawDb>();
            return db.LoadSingleQueryRow();
        }

        [HttpGet("ef")]
        [Produces("application/json")]
        public Task<World> Ef()
        {
            var db = HttpContext.RequestServices.GetRequiredService<EfDb>();
            return db.LoadSingleQueryRow();
        }
    }
}
