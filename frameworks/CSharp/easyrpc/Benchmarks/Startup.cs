using EasyRpc.AspNetCore.Serializers;
using EasyRpc.AspNetCore.Utf8Json;
using Microsoft.Extensions.Configuration;

namespace Benchmarks
{
    using Benchmarks.Data;
    using Benchmarks.Services;
    using EasyRpc.AspNetCore;
    using Microsoft.AspNetCore.Builder;
    using Microsoft.Extensions.DependencyInjection;

    public class Startup
    {
        private IConfiguration _configuration;

        public Startup(IConfiguration configuration)
        {
            _configuration = configuration;
        }       

        public void ConfigureServices(IServiceCollection services)
        {
            services.AddRpcServices(registerJsonSerializer: false);
            services.AddSingleton<IContentSerializer, Utf8JsonContentSerializer>();
            services.AddSingleton<IRawDb, RawDb>();

            var appSettings = new AppSettings();

            _configuration.Bind(appSettings);

            services.AddSingleton(appSettings);   

            // for views
            services.AddControllersWithViews();         
        }

        public void Configure(IApplicationBuilder app)
        {
            app.UseRpcServices(api =>
            {
                api.GetMethod("/plaintext", () => "Hello, World!").Raw("text/plain");

                api.GetMethod("/json", () => new { message = "Hello, World!" });

                api.Expose<QueryService>();
                api.Expose<FortuneService>();
            });
        }
    }
}
