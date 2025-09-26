using Funq;
using ServiceStack;
using ServicestackV6.ServiceInterface;

[assembly: HostingStartup(typeof(ServicestackV6.AppHost))]

namespace ServicestackV6;

public class AppHost : AppHostBase, IHostingStartup
{
    public void Configure(IWebHostBuilder builder) => builder.Configure(app =>
    {
        if (!HasInit)
            app.UseServiceStack(new AppHost());
    });

    public AppHost() : base("ServicestackV6", typeof(MyServices).Assembly) { }

    public override void Configure(Container container)
    {
        SetConfig(new HostConfig
        {
            UseSameSiteCookies = true,
        });
    }
}
