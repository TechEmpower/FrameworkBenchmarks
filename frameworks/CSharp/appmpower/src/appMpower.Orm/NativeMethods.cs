using System.Runtime.InteropServices;
using System.Text; 
using System.Text.Json; 
using appMpower.Orm.Data; 
using appMpower.Orm.Objects; 
using appMpower.Orm.Serializers; 

namespace appMpower.Orm;

public static class NativeMethods
{
    private static JsonWriterOptions _jsonWriterOptions = new JsonWriterOptions
    {
        Indented = false, 
        SkipValidation = true
    };

    private readonly static WorldSerializer _worldSerializer = new WorldSerializer();
    private readonly static WorldsSerializer _worldsSerializer = new WorldsSerializer();

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
        //var world = RawDb.LoadSingleQueryRow().GetAwaiter().GetResult();
        var world = RawDb.LoadSingleQueryRow();

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

    [UnmanagedCallersOnly(EntryPoint = "Fortunes")]
    public static unsafe IntPtr Fortunes(int* length, IntPtr* handlePointer)
    {
        //List<Fortune> fortunes = RawDb.LoadFortunesRows().GetAwaiter().GetResult(); 
        List<Fortune> fortunes = RawDb.LoadFortunesRows(); 
        string fortunesView = FortunesView.Render(fortunes);
        byte[] byteArray = Encoding.UTF8.GetBytes(fortunesView);

        //*length = Encoding.UTF8.GetByteCount(fortunesView); //fortunesView.Length + 32; 
        *length = byteArray.Length; 

        GCHandle handle = GCHandle.Alloc(byteArray, GCHandleType.Pinned);
        IntPtr byteArrayPointer = handle.AddrOfPinnedObject();
        *handlePointer = GCHandle.ToIntPtr(handle);

        return byteArrayPointer;
    }

    [UnmanagedCallersOnly(EntryPoint = "Query")]
    public static unsafe IntPtr Query(int queries, int* length, IntPtr* handlePointer)
    //public static unsafe byte* Query(int queries, int* length)
    {
        World[] worlds = RawDb.ReadMultipleRows(queries);

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldsSerializer.Serialize(utf8JsonWriter, worlds);

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
