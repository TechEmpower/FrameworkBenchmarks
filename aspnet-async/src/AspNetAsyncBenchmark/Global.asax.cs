using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Http;
using System.Web.Mvc;
using System.Web.Routing;

namespace AspNetAsyncBenchmark
{
	public class MvcApplication 
		: System.Web.HttpApplication
	{
		protected void Application_Start()
		{
			//System.Net.ServicePointManager.DefaultConnectionLimit = int.MaxValue;
			AreaRegistration.RegisterAllAreas();

			WebApiConfig.Register(GlobalConfiguration.Configuration);
			FilterConfig.RegisterGlobalFilters(GlobalFilters.Filters);
			RouteConfig.RegisterRoutes(RouteTable.Routes);

			// use only C# Razor template engine
			var razor = ViewEngines.Engines.OfType<RazorViewEngine>().First();
			ViewEngines.Engines.Clear();
			ViewEngines.Engines.Add(razor);
		}
	}
}