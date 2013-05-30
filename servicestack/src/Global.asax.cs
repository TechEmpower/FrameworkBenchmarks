using System;
using System.Collections.Generic;
using System.Web;

namespace ServiceStackBenchmark
{
    public class Global : HttpApplication
    {
        protected void Application_Start()
        {
            AppHost.Start();
        }
    }
}