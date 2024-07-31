using System.Runtime.InteropServices;
using System.Text; 
using System.Text.Json; 
using appMpowerAot.Data; 
using appMpowerAot.DataObjects; 
using appMpowerAot.Serializers; 

namespace appMpowerAot;

public static class NativeMethods
{
    private readonly static JsonMessageSerializer _jsonMessageSerializer = new JsonMessageSerializer();
    private readonly static WorldSerializer _worldSerializer = new WorldSerializer();
    private static byte[][] _byteArrays = new byte[99][];

    [UnmanagedCallersOnly(EntryPoint = "Dbms")]
    public static void Dbms(int dbms)
    {
        Constants.Dbms = (Dbms)dbms; 
        DbProviderFactory.SetConnectionString();
    }

    [UnmanagedCallersOnly(EntryPoint = "DbProvider")]
    public static void DbProvider(int dbProvider)
    {
        Constants.DbProvider = (DbProvider)dbProvider; 
        DbProviderFactory.SetConnectionString();
    }
    
    [UnmanagedCallersOnly(EntryPoint = "HelloWorld")]
    public static unsafe char* HelloWorld()
    {
        string helloWorld = "Hello, World!"; 

        fixed(char* s = helloWorld)
        {
            return s; 
        }
    }

    [UnmanagedCallersOnly(EntryPoint = "JsonMessage")]
    public static unsafe byte* JsonMessage(int* length)
    {
        var jsonMessage = new JsonMessage
        {
            Message = "Hello, World!"
        };

        var jsonWriterOptions = new JsonWriterOptions
        {
            Indented = false, 
            SkipValidation = true
        };

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, jsonWriterOptions);

        _jsonMessageSerializer.Serialize(utf8JsonWriter, jsonMessage);
        //Console.WriteLine(utf8JsonWriter.BytesCommitted); 
        *length = (int)utf8JsonWriter.BytesCommitted; 

        fixed(byte* b = memoryStream.ToArray())
        {
            return b; 
        }
    }

    [UnmanagedCallersOnly(EntryPoint = "Db")]
    public static unsafe byte* Db(int* length)
    {
        var world = RawDb.LoadSingleQueryRow().GetAwaiter().GetResult();

        var jsonWriterOptions = new JsonWriterOptions
        {
            Indented = false, 
            SkipValidation = true
        };

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, jsonWriterOptions);

        _worldSerializer.Serialize(utf8JsonWriter, world);

        *length = (int)utf8JsonWriter.BytesCommitted; 

        fixed(byte* b = memoryStream.ToArray())
        {
            return b; 
        }
    }
}
