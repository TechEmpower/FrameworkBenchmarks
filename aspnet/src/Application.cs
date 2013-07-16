using System;
using System.Configuration;
using System.Threading;
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
            Threads();
        }

        private void Routes()
        {
            RouteTable.Routes.Clear();

            RouteTable.Routes.MapRoute(
                name: "JSON",
                url: "json/{action}",
                defaults: new { controller = "Json", action = "Default" }
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

        private void Threads()
        {
            // To improve CPU utilization, increase the number of threads that the .NET thread pool expands by when
            // a burst of requests come in. We could do this by editing machine.config/system.web/processModel/minWorkerThreads,
            // but that seems too global a change, so we do it in code for just our AppPool. More info:
            //
            // http://support.microsoft.com/kb/821268
            // http://blogs.msdn.com/b/tmarq/archive/2007/07/21/asp-net-thread-usage-on-iis-7-0-and-6-0.aspx
            // http://blogs.msdn.com/b/perfworld/archive/2010/01/13/how-can-i-improve-the-performance-of-asp-net-by-adjusting-the-clr-thread-throttling-properties.aspx

            int newMinWorkerThreads = Convert.ToInt32(ConfigurationManager.AppSettings["minWorkerThreadsPerLogicalProcessor"]);
            if (newMinWorkerThreads > 0)
            {
                int minWorkerThreads, minCompletionPortThreads;
                ThreadPool.GetMinThreads(out minWorkerThreads, out minCompletionPortThreads);
                ThreadPool.SetMinThreads(Environment.ProcessorCount * newMinWorkerThreads, minCompletionPortThreads);
            }
        }

        public void Dispose()
        {
        }
    }
}