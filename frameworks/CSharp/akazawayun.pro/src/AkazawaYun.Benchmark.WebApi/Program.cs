#pragma warning disable IDE1006,IL2026

using AkazawaYun.AOT;
using AkazawaYun.PRO7;

namespace AkazawaYun.Benchmark.WebApi;

class Program : IPostFunctionWrapper
{
    static readonly akaWebBuilder builder;
    static readonly akaDbFactory mysql;
    const int port = 8080;

    static Program()
    {
        akaJson.Config(null, AotJsonContext.Default);
        builder = akaWebBuilder.Shared.SetPort(port).SetDev()
            .Add<akaXmlSummary, akaXmlSummary>(() => null)
            .Build()
            .Config<IWebListener, akaHttpListenerVBase>(lis => lis.LogLevel = 0)
            .Config<IWebReceptor, akaWebInterceptor>(itc =>
            {
                itc.ClearInterceptor();
                itc.AddInterceptor(new akaWebInterceptorAsPost());
            });
        mysql = new akaDbBuilderII()
            .SetServer("tfb-database")
            //.SetServer("localhost:3306")
            .SetUser("benchmarkdbuser")
            //.SetUser("root")
            .SetPwd("benchmarkdbpass")
            //.SetPwd("123456")
            .SetDatabase("hello_world")
            .SetCharset()
            .SetOtherset()
            .Build<Mysql>();
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

public class akaWebInterceptorAsPost : WebInterceptor
{
    public override ValueTask<InterceptorHttpRes> Intercept(IHttpContext http)
    {
        http.Method = "POST";
        return InterceptorHttpRes.No();
    }
}
