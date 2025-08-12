using AkazawaYun.AOT;
using System.Text.Json.Serialization;

namespace AkazawaYun.FrameworkBenchmarks;


[JsonSerializable(typeof(JsonResponse))]
[JsonSerializable(typeof(World))]
public partial class Json : JsonSerializerContext { }


public class JsonResponse
{
    public string? message { get; set; }
}
public class World : IAotModel
{
    public int id { get; set; }
    public int randomNumber { get; set; }
}
