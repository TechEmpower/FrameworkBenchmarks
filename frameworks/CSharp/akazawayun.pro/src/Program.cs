#pragma warning disable IDE1006,IL2026

using AkazawaYun.AOT;
using AkazawaYun.PRO7;

namespace AkazawaYun.FrameworkBenchmarks;

class Program : IPostFunctionWrapper
{
    static readonly akzWebBuilder builder;
    static readonly akzDbFactory mysql;
    const int port = 2022;

    static Program()
    {
        akzJson.Config(null, AotJsonContext.Default);
        builder = akzWebBuilder.Shared.SetPort(port)
            .Build()
            .Config<IWebReceptor, akzWebInterceptor>(itc =>
            {
                itc.ClearInterceptor();
                itc.AddInterceptor(new akzWebInterceptorAsPost());
            });
        mysql = new akzDbBuilderII()
            .SetServer("tfb-database")
            //.SetServer("localhost")
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

        akzLog.Inf("[API SELF-TEST]");
        string url = $"http://localhost:{port}/plaintext";
        akzLog.Inf(" REQ URL :" + url);
        string res = await akzHttpClient.Shared.Get(url).FetchString();
        akzLog.Inf(" RES LEN :" + res.Length);
        akzLog.Inf(" RES BODY:" + res);
        akzLog.Inf("[OK, I WORK FINE]");

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


public class akzWebInterceptorAsPost : WebInterceptor
{
    public override ValueTask<InterceptorHttpRes> Intercept(IHttpContext http)
    {
        http.Method = "POST";
        return InterceptorHttpRes.No();
    }
}
