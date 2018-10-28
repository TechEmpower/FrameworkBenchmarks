using System;
using System.Threading;
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
            ThreadPool.GetMinThreads(out int workerThread, out int completionThread);
            // Double ThreadPool for non-async calls
            ThreadPool.SetMinThreads(workerThread * 2, completionThread);

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
            // app.UseResponseBuffering();
            app.UsePhp(new PhpRequestOptions(scriptAssemblyName: "Website"));
            // app.UseDefaultFiles();
            // app.UseStaticFiles();
        }
    }
}