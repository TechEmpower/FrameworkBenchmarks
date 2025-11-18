using AkazawaYun.PRO7;

namespace AkazawaYun.Benchmark.Platform;

class Program
{
    static readonly akzWebBuilder builder;

    static Program()
    {
        akzLog.War("AkazawaYun.PRO 平台压力测试特供版 ver2025.11.3, 只支持 /plaintext 和 /json");
        akzJson.Config(AotJsonContext.Default);
        builder = akzWebBuilder.Shared.Build(new MyBenchmarkReceptor());
    }
    static async Task Main()
    {
        await builder.Launch();
        await Task.Delay(-1);
    }
}
