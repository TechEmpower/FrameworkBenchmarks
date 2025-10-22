#pragma warning disable IDE1006,IL2026

using AkazawaYun.AOT;
using AkazawaYun.PRO7;
using AkazawaYun.PRO7.AkazawaYunWebFunctionAOP;
using AkazawaYun.PRO7.AkazawaYunWebInterceptor;
using System.Diagnostics.CodeAnalysis;

namespace AkazawaYun.FrameworkBenchmarks;

class Program : IPostFunctionWrapper
{
    static readonly akzWebBuilder builder;
    static readonly akzDbFactory mysql;


    static Program()
    {
        akzJson.Config(null, AotJsonContext.Default);
        builder = akzWebBuilder.Shared.SetDefault()
            .Build()
            .Config<IWebReceptor, akzWebInterceptor>(itc =>
            {
                itc.ClearInterceptor();
                itc.AddInterceptor(new akzWebInterceptorNotOnlyPost());
            });
        mysql = new akzDbBuilderII()
            .SetServer("tfb-database:3306")
            //.SetServer("localhost:3306")
            .SetUser("benchmarkdbuser")
            .SetPwd("benchmarkdbpass")
            .SetDatabase("hello_world")
            .SetCharset()
            .SetOtherset()
            .Build<Mysql>();
    }
    static async Task Main()
    {
        await builder.Launch();
        akzLog.Default = akzLog.Output.None;
        await Task.Delay(-1);
    }


    public static HttpRes plaintext() => HttpRes.HttpOK("Hello, World!", ".txt");
    public static JsonModel json() => new()
    {
        message = "Hello, World!"
    };

    //[WebFunctionAopTry]
    public static async Task<world> db()
    {
        await using IDb con = await mysql.Connect();
        world obj = await WorldService.GetRandomWorld(con);
        return obj;
    }
    //[WebFunctionAopTry]
    public static async Task<world[]> queries(string queries)
    {
        int count = ParseCount(queries);

        await using IDb con = await mysql.Connect();
        world[] lst = await WorldService.GetWorlds(con, count);
        return lst;
    }
    //[WebFunctionAopTry]
    public static async Task<world[]> updates(string queries)
    {
        int count = ParseCount(queries);

        await using IDb con = await mysql.Connect();
        world[] lst = await WorldService.GetWorlds(con, count);

        foreach (world obj in lst)
            obj.randomNumber = Random.Shared.Next(1, 10001);

        await WorldService.SaveWorlds(con, lst);

        return lst;
    }


    static int ParseCount(string queries)
    {
        if (!int.TryParse(queries, out int count))
            return 1;

        count = Math.Clamp(count, 1, 500);
        return count;
    }

}
