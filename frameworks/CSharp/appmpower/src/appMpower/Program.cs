using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Hosting;

namespace appMpower; 

class Program
{
    static void Main(string[] args)
    {
        BuildWebHost(args).Run();
    }

    static IHost BuildWebHost(string[] args)
    {
        var config = new ConfigurationBuilder()
            .AddJsonFile("appsettings.json")
            .AddEnvironmentVariables(prefix: "ASPNETCORE_")
            .AddCommandLine(args)
            .Build();

        var appSettings = config.GetSection("AppSettings").Get<AppSettings>();

        var host = Host.CreateDefaultBuilder(args)
            .ConfigureWebHostDefaults(webBuilder =>
            {
                webBuilder.UseConfiguration(config)
                          .UseKestrel(options =>
                          {
                            options.AddServerHeader = false; 
                            options.AllowSynchronousIO = true;
                          })
                          .UseStartup<Startup>();
            })
            .Build();

        return host;
    }
}

public class AppSettings
{
    public string Database { get; set; }
}
