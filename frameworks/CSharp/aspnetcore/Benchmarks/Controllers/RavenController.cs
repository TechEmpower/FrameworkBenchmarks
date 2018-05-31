using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Benchmarks.Data;
using Microsoft.AspNetCore.Mvc;

namespace Benchmarks.Controllers
{
    [Route("mvc/raven")]
    public class RavenController : Controller
    {
        private readonly RavenDb _db;

        public RavenController(RavenDb provider)
        {
            _db = provider;
        }

        [HttpGet("single")]
        [Produces("application/json")]
        public Task<WorldRaven> Single()
        {
            return _db.LoadSingleQueryRow();
        }

        [HttpGet("queries")]
        [Produces("application/json")]
        public Task<IEnumerable<WorldRaven>> Queries(int queries = 1)
        {
            queries = queries < 1 ? 1 : queries > 500 ? 500 : queries;
            return _db.LoadMultipleQueriesRows(queries);
        }

        [HttpGet("update")]
        [Produces("application/json")]
        public Task<IEnumerable<WorldRaven>> Update(int queries = 1)
        {
            queries = queries < 1 ? 1 : queries > 500 ? 500 : queries;
            return _db.LoadMultipleUpdatesRows(queries);
        }

        [HttpGet("fortunes")]
        public async Task<IActionResult> Fortunes()
        {
            return View("Fortunes", await _db.LoadFortunesRows());
        }
    }
}
