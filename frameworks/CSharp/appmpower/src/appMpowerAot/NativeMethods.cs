using System.Runtime.InteropServices;
using System.Text; 
using System.Text.Json; 
using appMpowerAot.Data; 
using appMpowerAot.DataObjects; 
using appMpowerAot.Serializers; 

namespace appMpowerAot;

public static class NativeMethods
{
    private static JsonWriterOptions _jsonWriterOptions = new JsonWriterOptions
    {
        Indented = false, 
        SkipValidation = true
    };

    private readonly static WorldSerializer _worldSerializer = new WorldSerializer();

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
    
    [UnmanagedCallersOnly(EntryPoint = "FreeHandlePointer")]
    public static void FreeHandlePointer(IntPtr handlePointer)
    {
        GCHandle handle = GCHandle.FromIntPtr(handlePointer);
        handle.Free();
    }

    [UnmanagedCallersOnly(EntryPoint = "Db")]
    public static unsafe IntPtr Db(int* length, IntPtr* handlePointer)
    {
        var world = RawDb.LoadSingleQueryRow().GetAwaiter().GetResult();

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldSerializer.Serialize(utf8JsonWriter, world);

        *length = (int)utf8JsonWriter.BytesCommitted; 
        byte[] byteArray = memoryStream.ToArray();

        GCHandle handle = GCHandle.Alloc(byteArray, GCHandleType.Pinned);
        // return the managed and byteArrayPointer pointer
        IntPtr byteArrayPointer = handle.AddrOfPinnedObject();
        *handlePointer = GCHandle.ToIntPtr(handle);

        return byteArrayPointer;

        /*
        fixed(byte* b = memoryStream.ToArray())
        {
            return b; 
        }
        */
    }
}
