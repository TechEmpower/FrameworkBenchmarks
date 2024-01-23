using EasyRpc.AspNetCore.Serializers;
using EasyRpc.AspNetCore.Utf8Json;
using Microsoft.Extensions.Configuration;

namespace Benchmarks
{
    using System.Text.Encodings.Web;
    using System.Text.Unicode;
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
            services.AddRpcServices(c => c.RegisterJsonSerializer = false);
            services.AddSingleton<IContentSerializer, Utf8JsonContentSerializer>();
            services.AddSingleton<IRawDb, RawDb>();

            var appSettings = new AppSettings();

            _configuration.Bind(appSettings);

            services.AddSingleton(appSettings);   

            // for views
            services.AddControllersWithViews();                
            var settings = new TextEncoderSettings(UnicodeRanges.BasicLatin, 
                            UnicodeRanges.Katakana,
                            UnicodeRanges.Hiragana);

            settings.AllowCharacter('\u2014');  // allow EM DASH through
            services.AddWebEncoders((options) =>  options.TextEncoderSettings = settings);     
        }

        public void Configure(IApplicationBuilder app)
        {
            app.UseRpcRouting(api =>
            {
                api.Method.Get("/plaintext", () => "Hello, World!").Raw("text/plain");

                api.Method.Get("/json", () => new { message = "Hello, World!" });

                api.Expose<QueryService>();
                api.Expose<FortuneService>();
            });
        }
    }
}
