using System.Runtime.InteropServices;
using System.Text; 
using System.Text.Json; 
using appMpower.Orm.Objects; 
using appMpower.Orm.Serializers; 

namespace appMpower.Orm;

public static class DotnetMethods
{
    private static JsonWriterOptions _jsonWriterOptions = new JsonWriterOptions
    {
        Indented = false, 
        SkipValidation = true
    };

    private readonly static WorldSerializer _worldSerializer = new WorldSerializer();

    public static byte[] Db()
    {
        var world = RawDb.LoadSingleQueryRow().GetAwaiter().GetResult();

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldSerializer.Serialize(utf8JsonWriter, world);

        return memoryStream.ToArray();
    }
}