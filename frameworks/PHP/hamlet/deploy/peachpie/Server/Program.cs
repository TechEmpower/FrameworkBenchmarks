using System.IO;
using System.Threading;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Rewrite;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Peachpie.AspNetCore.Web;

namespace Server
{
    class Program
    {
        static void Main(string[] args)
        {
            ThreadPool.GetMinThreads(out int workerThread, out int completionThread);
            ThreadPool.SetMinThreads(workerThread * 2, completionThread);

            var host = new WebHostBuilder()
                .UseKestrel()
                .UseUrls("http://0.0.0.0:8080")
                .UseStartup<Startup>()
                .Build();

            host.Run();
        }
    }

    class Startup
    {
        public void Configure(IApplicationBuilder app, IHostingEnvironment env, ILoggerFactory loggerFactory)
        {
            // loggerFactory.AddConsole();

            app.UseRewriter(new RewriteOptions().Add(ShortUrlRule));
            void ShortUrlRule(RewriteContext context)
            {
                var req = context.HttpContext.Request;
                req.Path = new PathString("/index.php");
                context.Result = RuleResult.SkipRemainingRules;
            }
            app.UsePhp(new PhpRequestOptions(scriptAssemblyName: "Website"));
        }
    }
}
