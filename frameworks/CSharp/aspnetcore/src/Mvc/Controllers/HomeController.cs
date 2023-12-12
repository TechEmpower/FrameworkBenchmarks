// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using Microsoft.AspNetCore.Mvc;

namespace Mvc.Controllers;

public class HomeController : Controller
{
    public IActionResult Index()
    {
        return View();
    }

    [HttpGet("plaintext")]
    public string Plaintext()
    {
        return "Hello, World!";
    }

    [HttpGet("json")]
    [Produces("application/json")]
    public object Json()
    {
        return new { message = "Hello, World!" };
    }
}
