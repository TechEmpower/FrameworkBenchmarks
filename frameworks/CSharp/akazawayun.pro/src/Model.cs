#pragma warning disable IDE1006,CS8981
using AkazawaYun.AOT;
using System.Text.Json.Serialization;

namespace AkazawaYun.FrameworkBenchmarks;


[JsonSerializable(typeof(JsonModel))]
[JsonSerializable(typeof(world[]))]
public partial class AotJsonContext : JsonSerializerContext { }


public class JsonModel
{
    public string? message { get; set; }
}
public class world : IAotModel
{
    public int id { get; set; }
    public int randomNumber { get; set; }
}
