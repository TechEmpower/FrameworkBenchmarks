using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using System;

namespace PlatformBenchmarks
{
    class Program
    {
        public static void Main(string[] args)
        {
            new HostBuilder().ConfigureServices(delegate (HostBuilderContext hostContext, IServiceCollection services)
            {
                services.AddHostedService<ZYHttpServer>();

            }).Build().Run();

        }
    }
}
