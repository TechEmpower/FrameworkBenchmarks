using System.Web.Mvc;
using System.Web.Routing;

namespace Benchmarks.Mono.AspNet
{
    public class MvcApplication : System.Web.HttpApplication
    {
        protected void Application_Start()
        {
            ViewEngines.Engines.Clear();
            ViewEngines.Engines.Add(new RazorViewEngine { ViewLocationFormats = new[] { "~/Views/{0}.cshtml" } });

            Routes();
        }

        private void Routes()
        {
            RouteTable.Routes.MapRoute(
                name: "JSON",
                url: "json",
                defaults: new { controller = "Json", action = "Index" }
            );

            RouteTable.Routes.MapRoute(
                name: "ADO.NET/MySQL/DB",
                url: "adonet/mysql",
                defaults: new { controller = "AdoNetMySql", action = "Index" }
            );

            RouteTable.Routes.MapRoute(
                name: "ADO.NET/MySQL/Fortunes",
                url: "adonet/mysql/fortunes",
                defaults: new { controller = "AdoNetMySql", action = "Fortunes" }
            );

            RouteTable.Routes.MapRoute(
                name: "ADO.NET/PostgreSQL",
                url: "adonet/postgresql",
                defaults: new { controller = "AdoNetPostgreSql", action = "Index" }
            );

            RouteTable.Routes.MapRoute(
                name: "ADO.NET/PostgreSQL/Fortunes",
                url: "adonet/postgresql/fortunes",
                defaults: new { controller = "AdoNetPostgreSql", action = "Fortunes" }
            );

            RouteTable.Routes.MapRoute(
                name: "EntityFramework/MySQL",
                url: "entityframework/mysql",
                defaults: new { controller = "EntityFrameworkMySql", action = "Index" }
            );

            RouteTable.Routes.MapRoute(
                name: "EntityFramework/MySQL/Fortunes",
                url: "entityframework/mysql/fortunes",
                defaults: new { controller = "EntityFrameworkMySql", action = "Fortunes" }
            );

            RouteTable.Routes.MapRoute(
                name: "MongoDB",
                url: "mongodb",
                defaults: new { controller = "MongoDB", action = "Index" }
            );
        }
    }
}