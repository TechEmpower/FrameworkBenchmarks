using System.Runtime.InteropServices;
using System.Text; 
using System.Text.Json; 
using appMpower.Orm.Data; 
using appMpower.Orm.Objects; 
using appMpower.Orm.Serializers; 

namespace appMpower.Orm;

//These methods are for test purposes only; not used in actual execution
public static class DotnetMethods
{
    private static JsonWriterOptions _jsonWriterOptions = new JsonWriterOptions
    {
        Indented = false, 
        SkipValidation = true
    };

    private readonly static WorldSerializer _worldSerializer = new WorldSerializer();
    private readonly static WorldsSerializer _worldsSerializer = new WorldsSerializer();

    public static byte[] Db()
    {
        var world = RawDb.LoadSingleQueryRow().GetAwaiter().GetResult();

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldSerializer.Serialize(utf8JsonWriter, world);

        return memoryStream.ToArray();
    }

    public static byte[] Query(int queries)
    {
        World[] worlds = RawDb.ReadMultipleRows(queries).GetAwaiter().GetResult();

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldsSerializer.Serialize(utf8JsonWriter, worlds);

        return memoryStream.ToArray();
    }

    public static byte[] Updates(int count)
    {
        World[] worlds = RawDb.LoadMultipleUpdatesRows(count).GetAwaiter().GetResult();

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldsSerializer.Serialize(utf8JsonWriter, worlds);

        return memoryStream.ToArray();
    }

    public static byte[] Fortunes()
    {
        List<Fortune> fortunes = RawDb.LoadFortunesRows().GetAwaiter().GetResult(); 
        string fortunesView = FortunesView.Render(fortunes);
        byte[] byteArray = Encoding.UTF8.GetBytes(fortunesView);

        return byteArray.ToArray();
    }
}