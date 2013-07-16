using System;
using System.Configuration;
using System.Threading;
using System.Web;

public class MyModule: IHttpModule
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
        Threads();
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
