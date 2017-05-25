using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using System.Web.Routing;

namespace AspNetAsyncBenchmark
{
	public class RouteConfig
	{
		public static void RegisterRoutes(RouteCollection routes)
		{
			routes.IgnoreRoute("{resource}.axd/{*pathInfo}");

			routes.MapRoute(name: "json-serialize", url: "json/{action}", defaults: new { controller = "Json", action = "Serialize" });
			routes.MapRoute(name: "db-single", url: "db", defaults: new { controller = "Db", action = "SingleQuery" });
			routes.MapRoute(name: "db-multiple", url: "queries", defaults: new { controller = "Db", action = "MultipleQueries" });
			routes.MapRoute(name: "db-fortunes", url: "fortunes", defaults: new { controller = "Db", action = "Fortunes" });
			routes.MapRoute(name: "db-updates", url: "updates", defaults: new { controller = "Db", action = "Updates" });
			routes.MapRoute(name: "plaintext", url: "plaintext", defaults: new { controller = "PlainText", action = "Index" });

			routes.MapRoute(
				 name: "Default",
				 url: "{controller}/{action}/{id}",
				 defaults: new { controller = "Home", action = "Index", id = UrlParameter.Optional }
			);
		}
	}
}