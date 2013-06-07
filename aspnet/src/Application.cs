using System.Web;
using System.Web.Mvc;
using System.Web.Routing;

namespace Benchmarks.AspNet
{
    public class Application : IHttpModule
    {
        private static volatile bool started = false;
        private static object locker = new object();

        public void Init(HttpApplication context)
        {
            if (!started)
            {
                lock (locker)
                {
                    Start();
                    started = true;
                }
            }
        }

        private void Start()
        {
            Routes();
            Views();
        }

        private void Routes()
        {
            RouteTable.Routes.Clear();

            RouteTable.Routes.MapRoute(
                name: "JSON",
                url: "json",
                defaults: new { controller = "Json", action = "Index" }
            );

            RouteTable.Routes.MapRoute(
                name: "WithProviders",
                url: "{controller}/{providerName}/{action}",
                defaults: new { action = "Index" },
                constraints: new { controller = "ado|entityframework", providerName = "mysql|postgresql|sqlserver" }
            );

            RouteTable.Routes.MapRoute(
                name: "Default",
                url: "{controller}/{action}",
                defaults: new { controller = "Home", action = "Index" }
            );
        }

        private void Views()
        {
            ViewEngines.Engines.Clear();
            ViewEngines.Engines.Add(new RazorViewEngine { ViewLocationFormats = new[] { "~/Views/{0}.cshtml" } });
        }

        public void Dispose()
        {
        }
    }
}