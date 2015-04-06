using System;
using System.Collections.Generic;
using System.Web;
using Nancy;
using System.Threading;

namespace NancyBenchmark
{
    public class Global : HttpApplication
    {
        protected void Application_Start()
        {
            var threads = 40 * Environment.ProcessorCount;
            ThreadPool.SetMaxThreads(threads, threads);
            ThreadPool.SetMinThreads(threads, threads);
        }
    }
}