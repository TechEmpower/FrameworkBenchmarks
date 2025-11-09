#pragma warning disable IDE1006,CS8981
using System.Text.Json.Serialization;

namespace AkazawaYun.Benchmark.Platform;


[JsonSerializable(typeof(JsonModel))]
public partial class AotJsonContext : JsonSerializerContext { }


public class JsonModel
{
    public string? message { get; set; }
}
