using AkazawaYun.PRO7;
using System.Text.Json.Serialization;

namespace AkazawaYun.FrameworkBenchmarks;

public class Program : IPostFunction
{
    static async Task Main()
    {
        akzJson.Config(null, Json.Default);
        var server = await akzWebBuilder.Shared.Load().SetDefault().Build().Launch();
        akzLog.Default = akzLog.Output.None;
        await Task.Delay(-1);
    }


    public static async ValueTask<HttpRes> plaintext(HttpReq _)
    {
        return "Hello, World!";
    }
    public static async ValueTask<HttpRes> json(HttpReq _)
    {
        return HttpRes.HttpJson(new JsonResponse
        {
            Message = "Hello, World!"
        });
    }
}

public class JsonResponse
{
    public string? Message { get; set; }
}

[JsonSerializable(typeof(JsonResponse))]
public partial class Json : JsonSerializerContext { }
