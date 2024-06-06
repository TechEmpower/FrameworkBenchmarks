using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using System;
using System.IO;

namespace PlatformBenchmarks
{
    class Program
    {

        //public static bool Debug = false;

        public static bool UpDB = false;

        public static void Main(string[] args)
        {
            //Debug = (args != null && args.Length > 0 && args[0] == "debug");
            var data = GMTDate.Default.DATE;
            UpDB = (args != null && args.Length > 0 && args[0] == "updb");
            UpdateCommandsCached.Init();
            //HttpServer server = new HttpServer();
            //server.StartAsync(default);
            //Console.ReadLine();
            new HostBuilder().ConfigureServices(delegate (HostBuilderContext hostContext, IServiceCollection services)
            {
                services.AddHostedService<HttpServer>();
            }).Build().Run();
        }
    }
}
