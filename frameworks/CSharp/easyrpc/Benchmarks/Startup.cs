using EasyRpc.AspNetCore.Serializers;
using EasyRpc.AspNetCore.Utf8Json;

namespace Benchmarks
{
    using EasyRpc.AspNetCore;
    using Microsoft.AspNetCore.Builder;
    using Microsoft.Extensions.DependencyInjection;

    public class Startup
    {
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddRpcServices(registerJsonSerializer: false);
            services.AddSingleton<IContentSerializer, Utf8JsonContentSerializer>();
        }

        public void Configure(IApplicationBuilder app)
        {
            app.UseRpcServices(api =>
            {
                api.GetMethod("/plaintext", () => "Hello, World!").Raw("text/plain");

                api.GetMethod("/json", () => new { message = "Hello, World!" });
            });
        }
    }
}
