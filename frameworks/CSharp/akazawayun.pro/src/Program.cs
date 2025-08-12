using AkazawaYun.AOT;
using AkazawaYun.PRO7;
using AkazawaYun.PRO7.AkazawaYunWebFunctionAOP;
using AkazawaYun.PRO7.AkazawaYunWebInterceptor;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.Intrinsics.Arm;
using System.Text.Json.Serialization;
using System.Threading.Tasks;

namespace AkazawaYun.FrameworkBenchmarks;

internal class Program : IPostFunctionWrapper
{
    static readonly akzWebBuilder builder;
    static readonly akzDbFactory mysql;


    static Program()
    {
        akzJson.Config(null, Json.Default);
        builder = akzWebBuilder.Shared.SetDefault()
            .Build()
            .Config<IWebReceptor, akzWebInterceptor>(itc =>
            {
                itc.AddInterceptor(new akzWebInterceptorNotOnlyPost());
            });
        mysql = new akzDbBuilderII()
            .SetServer("tfb-database")
            .SetDatabase("hello_world")
            .SetUser("benchmarkdbuser")
            .SetPwd("benchmarkdbpass")
            .SetOtherset("Maximum Pool Size=1024;SslMode=None;ConnectionReset=false;ConnectionIdlePingTime=900;ConnectionIdleTimeout=0;AutoEnlist=false;DefaultCommandTimeout=0;ConnectionTimeout=0;IgnorePrepare=false;")
            .Build<Mysql>();
    }
    static async Task Main()
    {
        await builder.Launch();
        akzLog.Default = akzLog.Output.None;
        await Task.Delay(-1);
    }



    public static HttpRes plaintext() => HttpRes.HttpOK("Hello, World!", ".txt");
    public static JsonResponse json() => new()
    {
        message = "Hello, World!"
    };
    [WebFunctionAopTry]
    public static async Task<World> db()
    {
        await using IDb con = await mysql.Connect();
        World obj = await WorldService.GetRandomWorld(con);
        return obj;
    }
    [WebFunctionAopTry]
    public static async Task<World[]> queries(string queries)
    {
        if (!int.TryParse(queries, out int count))
            count = 1;
        count = Math.Clamp(count, 1, 500);

        await using IDb con = await mysql.Connect();
        var lst = await WorldService.GetWorlds(con, count);
        return lst;
    }
    [WebFunctionAopTry]
    public static async Task<World[]> updates(string queries)
    {
        if (!int.TryParse(queries, out int count))
            count = 1;
        count = Math.Clamp(count, 1, 500);

        await using IDb con = await mysql.Connect();
        var lst = await WorldService.GetWorlds(con, count);

        foreach (var item in lst)
            item.randomNumber = Random.Shared.Next(1, 10001);

        await WorldService.SaveWorlds(con, lst);

        return lst;
    }


}
