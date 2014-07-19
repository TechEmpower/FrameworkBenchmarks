using System;
using Nancy.Hosting.Event2;
using Nancy;
using System.Threading;

namespace LibeventHost
{
    internal class MainClass
    {
        public static void Main(string[] args)
        {
            var host = args[0];
            var port = int.Parse(args[1]);
            var dbHost = args[2];
            LibLocator.Init();
            NancyModules.DbModule.MYSQL_CONNECTION_STRING = "server=localhost;user id=benchmarkdbuser;password=benchmarkdbpass;database=hello_world"
                .Replace("localhost", dbHost);
            AddToIndex();
            var threads = 20*Environment.ProcessorCount;
            ThreadPool.SetMaxThreads(threads, threads);
            ThreadPool.SetMinThreads(threads, threads);
            new NancyEvent2Host(host, port, new DefaultNancyBootstrapper()).Start();

        }

        private static void AddToIndex()
        {
            Nancy.Bootstrapper.AppDomainAssemblyTypeScanner
                 .AddAssembliesToScan(typeof(NancyModules.JsonModule).Assembly);
        }
    }
}