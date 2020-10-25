using System;
using System.Threading;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Features;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;

namespace PeachpieBenchmarks.Server
{
    class Program
    {
        static void Main(string[] args)
        {
            // Double ThreadPool for non-async calls
            ThreadPool.GetMinThreads(out int workerThread, out int completionThread);
            ThreadPool.SetMinThreads(workerThread * 2, completionThread);

            // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview

            new WebHostBuilder()
                .UseKestrel(options =>
                {
                    options.AddServerHeader = true; // tfb requires "Server" header
                    //options.Limits.KeepAliveTimeout = Timeout.InfiniteTimeSpan; // default 2:00
                    //options.Limits.MaxConcurrentConnections = 1000;
                })
                .UseUrls("http://*:8080/")
                .UseStartup<Startup>()
                .Build()
                .Run();
        }
    }

    class Startup
    {
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddPhp(options =>
            {
                // disable timeout
                options.Core.ExecutionTimeout = 0;
            });
        }

        public void Configure(IApplicationBuilder app)
        {
            //// disable response buffering and chunked transfer
            //app.Use((httpcontext, next) =>
            //{
            //    var responsefeature = httpcontext.Features.Get<IHttpResponseBodyFeature>();
            //    responsefeature?.DisableBuffering();

            //    //
            //    return next();
            //});

            // app.UseResponseBuffering();
            app.UsePhp(new PhpRequestOptions(scriptAssemblyName: "Website"));
            // app.UseDefaultFiles();
            // app.UseStaticFiles();
        }
    }
}