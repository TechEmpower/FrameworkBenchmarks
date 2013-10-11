using System;
using System.Threading;
using System.Web;

namespace ServiceStackBenchmark
{
    public class Global : HttpApplication
    {
        protected void Application_Start(object sender, EventArgs e)
        {
            AppHostConfigHelper.ConfigThreadPool();

            new AppHost().Init();
        }

        protected void Application_BeginRequest(object src, EventArgs e)
        { }

        protected void Application_EndRequest(object src, EventArgs e)
        { }
    }
}