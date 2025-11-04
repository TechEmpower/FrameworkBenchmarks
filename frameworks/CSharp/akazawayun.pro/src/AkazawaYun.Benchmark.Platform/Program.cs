using AkazawaYun.PRO7;

namespace AkazawaYun.Benchmark.Platform;

class Program
{
    static readonly akzWebBuilder builder;
    const int port = 8080;

    static Program()
    {
        akzLog.War("AkazawaYun.PRO 压力测试特供版 ver2025.11.3, 只支持 /plaintext 和 /json");
        akzJson.Config(null, AotJsonContext.Default);
        builder = akzWebBuilder.Shared.SetPort(port).SetDev(true)
            .Add<akzXmlSummary, akzXmlSummary>(() => null)
            .Add<IWebReceptor, MyBenchmarkReceptor>(() => new MyBenchmarkReceptor())
            .Add<IWebListener, akzHttpListenerVIBenchmark>(() => new(port)
            {
                LogLevel = 0
            }).Build();
    }
    static async Task Main()
    {
        await builder.Launch();

        akzLog.Inf("[API SELF-TEST]");
        string url = $"http://localhost:{port}/plaintext";
        akzLog.Inf(" REQ URL :" + url);
        string res = await akzHttpClient.Shared.Get(url).FetchString();
        akzLog.Inf(" RES LEN :" + res.Length);
        akzLog.Inf(" RES BODY:" + res);
        akzLog.Inf("[OK, I WORK FINE]");

        akzLog.Default = akzLog.Output.NoneButWar;
        await Task.Delay(-1);
    }
}
