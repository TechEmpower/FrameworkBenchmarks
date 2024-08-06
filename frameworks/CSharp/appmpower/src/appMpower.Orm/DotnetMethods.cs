using System.Runtime.InteropServices;
using System.Text; 
using System.Text.Json; 
using appMpower.Orm.Data; 
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
    private readonly static WorldsSerializer _worldsSerializer = new WorldsSerializer();

    public static byte[] Db()
    {
        Constants.Dbms = Dbms.PostgreSQL; 
        Constants.DbProvider = DbProvider.ODBC; 
        DbProviderFactory.SetConnectionString();

        //var world = RawDb.LoadSingleQueryRow().GetAwaiter().GetResult();
        var world = RawDb.LoadSingleQueryRow();

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldSerializer.Serialize(utf8JsonWriter, world);

        return memoryStream.ToArray();
    }

    public static byte[] Query(int queries)
    {
        Constants.Dbms = Dbms.PostgreSQL; 
        Constants.DbProvider = DbProvider.ODBC; 
        DbProviderFactory.SetConnectionString();

        World[] worlds = RawDb.ReadMultipleRows(queries);

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldsSerializer.Serialize(utf8JsonWriter, worlds);

        return memoryStream.ToArray();
    }
}