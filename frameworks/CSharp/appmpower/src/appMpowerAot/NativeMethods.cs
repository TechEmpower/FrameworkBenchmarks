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
    //private static byte[][] _byteArrays = new byte[99][];
    //private static GCHandle[] _handles = new GCHandle[99];

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
    //public static unsafe byte* JsonMessage(int* length)
    public static unsafe IntPtr JsonMessage(int* length, IntPtr* unmanagedPointer)
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
        *length = (int)utf8JsonWriter.BytesCommitted; 

        /*
        fixed(byte* b = memoryStream.ToArray())
        {
            return b; 
        }
        */

        byte[] byteArray = memoryStream.ToArray();

        GCHandle handle = GCHandle.Alloc(byteArray, GCHandleType.Pinned);
        // return the managed and unmanaged pointer
        IntPtr managedPointer = handle.AddrOfPinnedObject();
        *unmanagedPointer = GCHandle.ToIntPtr(handle);

        //Console.WriteLine(managedPointer.ToString());
        //Console.WriteLine(GCHandle.ToIntPtr(handle).ToString());
        return managedPointer;
    }

    [UnmanagedCallersOnly(EntryPoint = "FreeUnmanagedPointer")]
    public static void FreeUnmanagedPointer(IntPtr unmanagedPointer)
    {
        GCHandle handle = GCHandle.FromIntPtr(unmanagedPointer);
        handle.Free();
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
