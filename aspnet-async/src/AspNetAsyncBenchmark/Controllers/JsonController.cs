using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;

using JSON = Jil.JSON;

using AspNetAsyncBenchmark.Models;

namespace AspNetAsyncBenchmark.Controllers
{
	public class JsonController
		: Controller
	{
		public ActionResult Serialize()
		{
			return Content(JSON.Serialize(new JsonMessage { message = "Hello, World!" }), "application/json");
		}
	}
}