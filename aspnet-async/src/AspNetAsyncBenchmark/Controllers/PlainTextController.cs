using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;

namespace AspNetAsyncBenchmark.Controllers
{
	public class PlainTextController 
		: Controller
	{
		public ActionResult Index()
		{
			return Content("Hello, World!", "text/plain");
		}
	}
}