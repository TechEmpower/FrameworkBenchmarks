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
    private readonly static FortunesSerializer _fortunesSerializer = new FortunesSerializer();
    private static readonly byte[] _delimiter = new byte[] { 0xFF, 0xFF, 0xFF, 0xFF };


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

    /*
    [UnmanagedCallersOnly(EntryPoint = "Fortunes")]
    public static unsafe IntPtr Fortunes(int* length, IntPtr* handlePointer)
    {
        List<Fortune> fortunes = RawDb.LoadFortunesRows().GetAwaiter().GetResult(); 
        string fortunesView = FortunesView.Render(fortunes);
        byte[] byteArray = Encoding.UTF8.GetBytes(fortunesView);

        *length = byteArray.Length; 

        GCHandle handle = GCHandle.Alloc(byteArray, GCHandleType.Pinned);
        IntPtr byteArrayPointer = handle.AddrOfPinnedObject();
        *handlePointer = GCHandle.ToIntPtr(handle);

        return byteArrayPointer;
    }
    */

    [UnmanagedCallersOnly(EntryPoint = "Fortunes")]
    public static unsafe IntPtr Fortunes(int* length, IntPtr* handlePointer)
    {
        List<Fortune> fortunes = RawDb.LoadFortunesRows().GetAwaiter().GetResult(); 

        int totalSize = 0;

        foreach (var fortune in fortunes)
        {
            totalSize += sizeof(int) // for Id
                       + Encoding.UTF8.GetByteCount(fortune.Message ?? "") // for Message
                       + _delimiter.Length; // for delimiter
        }

        // Allocate the total buffer
        byte[] buffer = new byte[totalSize];
        int offset = 0;

        // Write each object to the buffer
        foreach (var fortune in fortunes)
        {
            // Write Id
            BitConverter.TryWriteBytes(buffer.AsSpan(offset, sizeof(int)), fortune.Id);
            offset += sizeof(int);

            // Write Message
            int descriptionLength = Encoding.UTF8.GetBytes(fortune.Message ?? "", buffer.AsSpan(offset));
            offset += descriptionLength;

            // Write Delimiter
            _delimiter.CopyTo(buffer, offset);
            offset += _delimiter.Length;
        }

        byte[] byteArray = buffer.ToArray();
        *length = byteArray.Length; 

        /*
        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _fortunesSerializer.Serialize(utf8JsonWriter, fortunes);

        byte[] byteArray = memoryStream.ToArray();
        *length = (int)utf8JsonWriter.BytesCommitted; 
        */

        GCHandle handle = GCHandle.Alloc(byteArray, GCHandleType.Pinned);
        IntPtr byteArrayPointer = handle.AddrOfPinnedObject();
        *handlePointer = GCHandle.ToIntPtr(handle);

        return byteArrayPointer;
    }

    [UnmanagedCallersOnly(EntryPoint = "Query")]
    public static unsafe IntPtr Query(int queries, int* length, IntPtr* handlePointer)
    {
        World[] worlds = RawDb.ReadMultipleRows(queries).GetAwaiter().GetResult();

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldsSerializer.Serialize(utf8JsonWriter, worlds);

        *length = (int)utf8JsonWriter.BytesCommitted; 
        byte[] byteArray = memoryStream.ToArray();

        GCHandle handle = GCHandle.Alloc(byteArray, GCHandleType.Pinned);
        IntPtr byteArrayPointer = handle.AddrOfPinnedObject();
        *handlePointer = GCHandle.ToIntPtr(handle);

        return byteArrayPointer;
    }

    [UnmanagedCallersOnly(EntryPoint = "Updates")]
    public static unsafe IntPtr Updates(int count, int* length, IntPtr* handlePointer)
    {
        World[] worlds = RawDb.LoadMultipleUpdatesRows(count).GetAwaiter().GetResult();

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldsSerializer.Serialize(utf8JsonWriter, worlds);

        *length = (int)utf8JsonWriter.BytesCommitted; 
        byte[] byteArray = memoryStream.ToArray();

        GCHandle handle = GCHandle.Alloc(byteArray, GCHandleType.Pinned);
        IntPtr byteArrayPointer = handle.AddrOfPinnedObject();
        *handlePointer = GCHandle.ToIntPtr(handle);

        return byteArrayPointer;
    }

    [UnmanagedCallersOnly(EntryPoint = "DbById")]
    public static unsafe IntPtr DbById(int id, int* length, IntPtr* handlePointer)
    {
        var world = RawDb.LoadSingleQueryRowById(id).GetAwaiter().GetResult();

        var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, _jsonWriterOptions);

        _worldSerializer.Serialize(utf8JsonWriter, world);

        *length = (int)utf8JsonWriter.BytesCommitted; 
        byte[] byteArray = memoryStream.ToArray();

        GCHandle handle = GCHandle.Alloc(byteArray, GCHandleType.Pinned);
        IntPtr byteArrayPointer = handle.AddrOfPinnedObject();
        *handlePointer = GCHandle.ToIntPtr(handle);

        return byteArrayPointer;
    }
}
