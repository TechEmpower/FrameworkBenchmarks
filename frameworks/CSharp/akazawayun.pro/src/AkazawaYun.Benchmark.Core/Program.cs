using AkazawaYun.PRO7;

namespace AkazawaYun.Benchmark.Platform;

class Program
{
    static readonly akaWebBuilder builder;

    static Program()
    {
        akaLog.War("AkazawaYun.PRO 平台压力测试特供版 ver2026.1.28, 只支持 /plaintext 和 /json");
        akaJson.Config(AotJsonContext.Default);
        builder = akaWebBuilder.Shared.Build(new MyBenchmarkReceptor());
    }
    static async Task Main()
    {
        await builder.Launch();
        await Task.Delay(-1);
    }
}
