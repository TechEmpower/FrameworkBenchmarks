using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text; 
using System.Text.Json;
using System.Threading.Tasks;
using appMpower.Orm.Data;
using appMpower.Orm.Objects; 
using appMpower.Orm.Serializers; 

namespace appMpower.Orm;

#if !AOTDLL

public static class DotnetMethods
{
    private static JsonWriterOptions _jsonWriterOptions = new JsonWriterOptions
    {
        Indented = false, 
        SkipValidation = true
    };

    private readonly static WorldSerializer _worldSerializer = new WorldSerializer();
    private readonly static WorldsSerializer _worldsSerializer = new WorldsSerializer();

    public static void Dbms(int dbms)
    {
        Constants.Dbms = (Dbms)dbms;
        DbFactory.SetConnectionString();
        DbFactory.SetInstance();
    }

    public static void DbProvider(int dbProvider)
    {
        Constants.DbProvider = (DbProvider)dbProvider;
        DbFactory.SetConnectionString();
        DbFactory.SetInstance();
    }

    public static async Task<byte[]> Db()
    {
        var world = await RawDb.LoadSingleQueryRowAsync();

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldSerializer.Serialize(utf8JsonWriter, world);

        return memoryStream.ToArray();
    }

    public static async Task<byte[]> DbById(int id)
    {
        var world = await RawDb.LoadSingleQueryRowByIdAsync(id);

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldSerializer.Serialize(utf8JsonWriter, world);

        return memoryStream.ToArray();
    }

    public static async Task<byte[]> Query(int queries)
    {
        World[] worlds = await RawDb.LoadMultipleQueriesRowsAsync(queries);

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldsSerializer.Serialize(utf8JsonWriter, worlds);

        return memoryStream.ToArray();
    }

    public static async Task<byte[]> Updates(int count)
    {
        World[] worlds = await RawDb.LoadMultipleUpdatesRowsAsync(count);

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldsSerializer.Serialize(utf8JsonWriter, worlds);

        return memoryStream.ToArray();
    }

    public static async Task<byte[]> Fortunes()
    {
        List<Fortune> fortunes = await RawDb.LoadFortunesRowsAsync();
        string fortunesView = FortunesView.Render(fortunes);
        byte[] byteArray = Encoding.UTF8.GetBytes(fortunesView);

        return byteArray.ToArray();
    }
}

#endif