using AkazawaYun.PRO7;
using AkazawaYun.PRO7.AkazawaYunWebInterceptor;

namespace AkazawaYun.Benchmark.Platform;

class Program
{
    const int port = 8080;
    static readonly akaWebBuilder builder;

    static Program()
    {
        akaLog.War("AkazawaYun.PRO 平台压力测试特供版 ver2026.1.15, 只支持 /plaintext 和 /json");
        akaJson.Config(null, AotJsonContext.Default);
        builder = akaWebBuilder.Shared.SetPort(port).SetDev(true)
          .Add<IWebReceptor, akaWebReceptorBenchmark>(() => new MyBenchmarkReceptor())
          .Add<IWebListener, akaHttpListenerVIBenchmark>(() => new(port))
          .Build()
          .Config<IWebListener, akaHttpListenerVIBenchmark>(lis => lis.LogLevel = 0)
          .Config<IWebReceptor, akaWebInterceptor>(rcp =>
          {
              rcp.ClearInterceptor();
              rcp.AddInterceptor(new akaWebInterceptorAllAsPost());
          });
    }
    static async Task Main()
    {
        await builder.Launch();

        akaLog.Inf("[API SELF-TEST]");
        string url = $"http://localhost:{port}/plaintext";
        akaLog.Inf(" REQ URL :" + url);
        string res = await akaHttpClient.Shared.Get(url).FetchString();
        akaLog.Inf(" RES LEN :" + res.Length);
        akaLog.Inf(" RES BODY:" + res);
        akaLog.Inf("[OK, I WORK FINE]");

        akaLog.Default = akaLog.Output.NoneButWar;
        await Task.Delay(-1);
    }
}
