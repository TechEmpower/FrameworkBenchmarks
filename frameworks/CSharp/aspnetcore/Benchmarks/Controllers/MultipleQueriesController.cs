// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Threading.Tasks;
using Benchmarks.Data;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks.Controllers
{
    [Route("mvc/queries")]
    public class MultipleQueriesController : Controller
    {
        private readonly RavenDb _db;

        public MultipleQueriesController()
        { }

        public MultipleQueriesController(RavenDb provider)
        {
            _db = provider;
        }

        [HttpGet("raw")]
        [Produces("application/json")]
        public Task<World[]> Raw(int queries = 1)
        {
            return ExecuteQuery<RawDb>(queries);
        }

        [HttpGet("dapper")]
        [Produces("application/json")]
        public Task<World[]> Dapper(int queries = 1)
        {
            return ExecuteQuery<DapperDb>(queries);
        }

        [HttpGet("ef")]
        [Produces("application/json")]
        public Task<World[]> Ef(int queries = 1)
        {
            return ExecuteQuery<EfDb>(queries);
        }

        private Task<World[]> ExecuteQuery<T>(int queries) where T : IDb
        {
            queries = queries < 1 ? 1 : queries > 500 ? 500 : queries;
            var db = HttpContext.RequestServices.GetRequiredService<T>();
            return db.LoadMultipleQueriesRows(queries);
        }

        [HttpGet("raven")]
        [Produces("application/json")]
        public Task<IEnumerable<WorldRaven>> Raven(int queries = 1)
        {
            queries = queries < 1 ? 1 : queries > 500 ? 500 : queries;
            return _db.LoadMultipleQueriesRows(queries);
        }
    }
}
