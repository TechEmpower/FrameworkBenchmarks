// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using Benchmarks.Data;
using Microsoft.AspNetCore.Mvc;

namespace Benchmarks.Controllers;

[Route("mvc/db")]
public class SingleQueryController : Controller
{
    [HttpGet("raw")]
    [Produces("application/json")]
    public Task<World> Raw()
    {
        return ExecuteQuery<RawDb>();
    }

    [HttpGet("dapper")]
    [Produces("application/json")]
    public Task<World> Dapper()
    {
        return ExecuteQuery<DapperDb>();
    }

    [HttpGet("ef")]
    [Produces("application/json")]
    public Task<World> Ef()
    {
        return ExecuteQuery<EfDb>();
    }

    private Task<World> ExecuteQuery<T>() where T : IDb
    {
        var db = HttpContext.RequestServices.GetRequiredService<T>();
        return db.LoadSingleQueryRow();
    }
}
