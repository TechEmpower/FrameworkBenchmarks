using System.Runtime.InteropServices;
using System.Text; 
using System.Text.Json; 

namespace nativeAOT;

public static class NativeMethods
{
    private readonly static JsonMessageSerializer _jsonMessageSerializer = new JsonMessageSerializer();
    private static byte[][] _byteArrays = new byte[99][];

    [UnmanagedCallersOnly(EntryPoint = "HelloWorld")]
    public static unsafe char* HellowWorld()
    {
        string helloWorld = "Hello, World!"; 

        fixed(char* s = helloWorld)
        {
            return s; 
        }
    }

    [UnmanagedCallersOnly(EntryPoint = "JsonMessage")]
    public static unsafe char* JsonMessage()
    {
        var jsonMessage = new JsonMessage
        {
            message = "Hello, World!"
        };

        var jsonWriterOptions = new JsonWriterOptions
        {
            Indented = false, 
            SkipValidation = true
        };

        using var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, jsonWriterOptions);

        _jsonMessageSerializer.Serialize(utf8JsonWriter, jsonMessage);

        fixed(char* s = Encoding.UTF8.GetString(memoryStream.ToArray()))
        {
            return s; 
        }
    }

    [UnmanagedCallersOnly(EntryPoint = "JsonMessage2")]
    public static unsafe byte* JsonMessage2()
    {
        var jsonMessage = new JsonMessage
        {
            message = "Hello, World!"
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

        fixed(byte* b = memoryStream.ToArray())
        {
            return b; 
        }
    }

    [UnmanagedCallersOnly(EntryPoint = "JsonMessage31")]
    public static unsafe int JsonMessage31(int managedThreadId)
    {
        var jsonMessage = new JsonMessage
        {
            message = "Hello, World!"
        };

        var jsonWriterOptions = new JsonWriterOptions
        {
            Indented = false, 
            SkipValidation = true
        };

        using var memoryStream = new MemoryStream();
        using var utf8JsonWriter = new Utf8JsonWriter(memoryStream, jsonWriterOptions);

        _jsonMessageSerializer.Serialize(utf8JsonWriter, jsonMessage);
        //Console.WriteLine(utf8JsonWriter.BytesCommitted); 
        utf8JsonWriter.Flush();
        _byteArrays[managedThreadId] = memoryStream.ToArray();
        return (int)utf8JsonWriter.BytesCommitted;
    }

   [UnmanagedCallersOnly(EntryPoint = "JsonMessage32")]
    public static unsafe byte* JsonMessage32(int managedThreadId)
    {
        fixed(byte* b = _byteArrays[managedThreadId])
        {
            return b; 
        }
    }
}