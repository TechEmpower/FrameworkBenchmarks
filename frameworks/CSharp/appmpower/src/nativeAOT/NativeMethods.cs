using System.Runtime.InteropServices;
using System.Text; 
using System.Text.Json; 

namespace nativeAOT;

public static class NativeMethods
{
    private readonly static JsonMessageSerializer _jsonMessageSerializer = new JsonMessageSerializer();

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
}
