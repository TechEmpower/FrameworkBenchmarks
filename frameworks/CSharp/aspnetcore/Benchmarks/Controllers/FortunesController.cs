// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading.Tasks;
using Benchmarks.Data;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks.Controllers
{
    [Route("mvc")]
    public class FortunesController : Controller
    {
        private RawDb _rawDb;
        private DapperDb _dapperDb;
        private EfDb _efDb;

        private RawDb RawDb
        {
            get
            {
                if (_rawDb == null)
                {
                    _rawDb = HttpContext.RequestServices.GetRequiredService<RawDb>();
                }

                return _rawDb;
            }
        }

        private DapperDb DapperDb
        {
            get
            {
                if (_dapperDb == null)
                {
                    _dapperDb = HttpContext.RequestServices.GetRequiredService<DapperDb>();
                }

                return _dapperDb;
            }
        }

        private EfDb EfDb
        {
            get
            {
                if (_efDb == null)
                {
                    _efDb = HttpContext.RequestServices.GetRequiredService<EfDb>();
                }

                return _efDb;
            }
        }

        [HttpGet("fortunes/raw")]
        public async Task<IActionResult> Raw()
        {
            return View("Fortunes", await RawDb.LoadFortunesRows());
        }

        [HttpGet("fortunes/dapper")]
        public async Task<IActionResult> Dapper()
        {
            return View("Fortunes", await DapperDb.LoadFortunesRows());
        }

        [HttpGet("fortunes/ef")]
        public async Task<IActionResult> Ef()
        {
            return View("Fortunes", await EfDb.LoadFortunesRows());
        }
    }
}
