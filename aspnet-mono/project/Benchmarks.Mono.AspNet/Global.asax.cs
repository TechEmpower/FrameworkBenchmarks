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
                name: "ADO.NET/MySQL",
                url: "adonet/mysql",
                defaults: new { controller = "AdoNetMySql", action = "Index" }
            );

            RouteTable.Routes.MapRoute(
                name: "ADO.NET/MySQL/Async",
                url: "adonet/mysql/async",
                defaults: new { controller = "AdoNetMySql", action = "Async" }
            );

            RouteTable.Routes.MapRoute(
                name: "ADO.NET/PostgreSQL",
                url: "adonet/postgresql",
                defaults: new { controller = "AdoNetPostgreSql", action = "Index" }
            );

            RouteTable.Routes.MapRoute(
                name: "ADO.NET/PostgreSQL/Async",
                url: "adonet/postgresql/async",
                defaults: new { controller = "AdoNetPostgreSql", action = "Async" }
            );

            RouteTable.Routes.MapRoute(
                name: "EntityFramework/MySQL",
                url: "ef/mysql",
                defaults: new { controller = "EntityFrameworkMySql", action = "Index" }
            );

            RouteTable.Routes.MapRoute(
                name: "EntityFramework/PostgreSQL",
                url: "ef/postgresql",
                defaults: new { controller = "EntityFrameworkPostgreSql", action = "Index" }
            );

            RouteTable.Routes.MapRoute(
                name: "MongoDB",
                url: "mongodb",
                defaults: new { controller = "MongoDB", action = "Index" }
            );
        }
    }
}