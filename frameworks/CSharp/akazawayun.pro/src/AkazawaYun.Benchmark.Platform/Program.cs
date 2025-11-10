using AkazawaYun.PRO7;

namespace AkazawaYun.Benchmark.Platform;

class Program
{
    static readonly akzWebBuilder builder;
    const int port = 8080;

    static Program()
    {
        akzLog.War("AkazawaYun.PRO 平台压力测试特供版 ver2025.11.3, 只支持 /plaintext 和 /json");
        akzJson.Config(null, AotJsonContext.Default);
        builder = akzWebBuilder.Shared.SetPort(port).SetDev(true)
            .Add<akzXmlSummary, akzXmlSummary>(() => null)
            .Add<IWebReceptor, MyBenchmarkReceptor>(() => new MyBenchmarkReceptor())
            .Add<IWebListener, akzHttpListenerVIBenchmark>(() => new(port))
            .Build();
    }
    static async Task Main()
    {
        await builder.Launch();

        Console.WriteLine("[API SELF-TEST]");
        string url = $"http://localhost:{port}/plaintext";
        Console.WriteLine(" REQ URL :" + url);
        string res = await akzHttpClient.Shared.Get(url).FetchString();
        Console.WriteLine(" RES LEN :" + res.Length);
        Console.WriteLine(" RES BODY:" + res);
        Console.WriteLine("[OK, I WORK FINE]");

        akzLog.Default = akzLog.Output.NoneButWar;
        await Task.Delay(-1);
    }
}
