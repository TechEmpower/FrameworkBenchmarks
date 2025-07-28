using AkazawaYun.PRO7;
using AkazawaYun.PRO7.AkazawaYunWebInterceptor;
using System.Text.Json.Serialization;

namespace AkazawaYun.FrameworkBenchmarks;

public class Program : IPostFunction
{
    static async Task Main()
    {
        akzJson.Config(null, Json.Default);
        var server = await akzWebBuilder.Shared.Load().SetDefault().Build()
            .Config<IWebReceptor, akzWebInterceptor>(itc =>
            {
                itc.AddInterceptor(new akzWebInterceptorNotOnlyPost());
            }).Launch();
        akzLog.Default = akzLog.Output.None;
        await Task.Delay(-1);
    }


    public static ValueTask<HttpRes> plaintext(HttpReq _)
    {
        return HttpRes.HttpOK("Hello, World!", ".txt");
    }
    public static ValueTask<HttpRes> json(HttpReq _)
    {
        return HttpRes.HttpJson(new JsonResponse
        {
            message = "Hello, World!"
        });
    }
}

public class JsonResponse
{
    public string? message { get; set; }
}

[JsonSerializable(typeof(JsonResponse))]
public partial class Json : JsonSerializerContext { }
