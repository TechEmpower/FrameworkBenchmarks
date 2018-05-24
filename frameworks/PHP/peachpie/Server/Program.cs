using System;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Peachpie.Web;

namespace MyWebsite.Server
{
    class Program
    {
        static void Main(string[] args)
        {
            var host = new WebHostBuilder()
                .UseKestrel()
                .UseUrls("http://*:8080/")
                .UseStartup<Startup>()
                .Build();

            host.Run();
        }
    }
	
	class Startup
    {
        public void Configure(IApplicationBuilder app)
        {
            app.UseResponseBuffering();
            app.UsePhp(new PhpRequestOptions(scriptAssemblyName: "Website"));
            app.UseDefaultFiles();
            app.UseStaticFiles();
        }
    }
}