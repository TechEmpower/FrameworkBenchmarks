// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using Microsoft.AspNetCore.Mvc;
using Mvc.Database;
using Mvc.Models;

namespace Mvc.Controllers;

public class SingleQueryController : Controller
{
    [Route("db")]
    [Produces("application/json")]
    public Task<World> Index([FromServices] Db db)
    {
        return db.LoadSingleQueryRow();
    }
}
