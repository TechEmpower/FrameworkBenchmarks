using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using System;

namespace PlatformBenchmarks
{
    class Program
    {

        //public static bool Debug = false;

        public static bool UpDB = false;

        public static void Main(string[] args)
        {
            //Debug = (args != null && args.Length > 0 && args[0] == "debug");
            UpDB = (args != null && args.Length > 0 && args[0] == "updb");
            new HostBuilder().ConfigureServices(delegate (HostBuilderContext hostContext, IServiceCollection services)
            {
                services.AddHostedService<HttpServer>();
            }).Build().Run();
        }
    }
}
