// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Text;
using Microsoft.AspNetCore.Mvc;

namespace Benchmarks.Controllers;

[Route("mvc")]
public sealed class HomeController : Controller
{
    [HttpGet("plaintext")]
    public IActionResult Plaintext()
    {
        return new PlainTextActionResult();
    }

    [HttpGet("json")]
    [Produces("application/json")]
    public object Json()
    {
        return new { message = "Hello, World!" };
    }

    [HttpGet("view")]
    public ViewResult Index()
    {
        return View();
    }

    private sealed class PlainTextActionResult : IActionResult
    {
        private static readonly byte[] _helloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");

        public Task ExecuteResultAsync(ActionContext context)
        {
            var response = context.HttpContext.Response;
            response.StatusCode = StatusCodes.Status200OK;
            response.ContentType = "text/plain";
            var payloadLength = _helloWorldPayload.Length;
            response.ContentLength = payloadLength;
            return response.Body.WriteAsync(_helloWorldPayload, 0, payloadLength);
        }
    }
}
