﻿// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading.Tasks;
using Benchmarks.Data;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks.Controllers
{
    [Route("mvc/fortunes")]
    public class FortunesController : Controller
    {
        [HttpGet("raw")]
        public async Task<IActionResult> Raw()
        {
            var db = HttpContext.RequestServices.GetRequiredService<RawDb>();
            return View("Fortunes", await db.LoadFortunesRows());
        }

        [HttpGet("dapper")]
        public async Task<IActionResult> Dapper()
        {
            var db = HttpContext.RequestServices.GetRequiredService<DapperDb>();
            return View("Fortunes", await db.LoadFortunesRows());
        }

        [HttpGet("ef")]
        public async Task<IActionResult> Ef()
        {
            var db = HttpContext.RequestServices.GetRequiredService<EfDb>();
            return View("Fortunes", await db.LoadFortunesRows());
        }
    }
}
