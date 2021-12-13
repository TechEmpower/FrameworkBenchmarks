namespace Nancy.Benchmark
{
    using System.IO;
    using Microsoft.AspNetCore.Hosting;
    using Microsoft.Extensions.Configuration;

    public class Program
    {
        public static void Main(string[] args)
        {
            var config = new ConfigurationBuilder()
                .AddJsonFile("hosting.json", optional: true)
               .AddEnvironmentVariables(prefix: "ASPNETCORE_")
               .AddCommandLine(args)
               .Build();

            var webHost = new WebHostBuilder()
                .UseContentRoot(Directory.GetCurrentDirectory())
                .UseConfiguration(config)
                .UseStartup<Startup>()
                .UseKestrel(o =>
                {
                    o.AllowSynchronousIO = true;
                })
                .Build();

            webHost.Run();
        }
    }
}
