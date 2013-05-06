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
                name: "WithProviders",
                url: "{controller}/{providerName}/{action}",
                defaults: new { action = "Index" }
            );
            
            RouteTable.Routes.MapRoute(
                name: "Default",
                url: "{controller}/{action}",
                defaults: new { action = "Index" }
            );
        }
    }
}