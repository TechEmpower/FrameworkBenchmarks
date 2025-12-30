// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using Microsoft.AspNetCore.Mvc;
using Mvc.Database;

namespace Mvc.Controllers;

public class FortunesController : Controller
{
    [Route("fortunes")]
    public async Task<IActionResult> Index([FromServices] Db db)
    {
        var fortunes = await db.LoadFortunesRows();

        return View(fortunes);
    }
}
