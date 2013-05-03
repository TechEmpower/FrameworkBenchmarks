using System.Web.Mvc;
using System.Web.Routing;

namespace Benchmarks.Mono.AspNet
{
    public class MvcApplication : System.Web.HttpApplication
    {
        protected void Application_Start()
        {
            RouteTable.Routes.MapRoute(
                name: "JSON",
                url: "json",
                defaults: new { controller = "Json", action = "Index" }
            );

            RouteTable.Routes.MapRoute(
                name: "MySQL/Raw",
                url: "mysql-raw",
                defaults: new { controller = "MySql", action = "Raw" }
            );

            RouteTable.Routes.MapRoute(
                name: "MySQL/EF",
                url: "mysql-ef",
                defaults: new { controller = "MySql", action = "EntityFramework" }
            );
        }
    }
}