// ReSharper disable always SuggestVarOrType_BuiltInTypes
// (var is avoided intentionally in this project so that concrete types are visible at call sites.)
// ReSharper disable always StackAllocInsideLoop

using System.Runtime.CompilerServices;
using System.Text.Json;
using Unhinged;

#pragma warning disable CA2014

namespace Platform;

[SkipLocalsInit]
internal static class Program
{
    public static void Main(string[] args)
    {
        var builder = UnhingedEngine
            .CreateBuilder()
            .SetPort(8080)
            .SetNWorkersSolver(() => Environment.ProcessorCount)  
            .SetBacklog(16384) 
            .SetMaxEventsPerWake(512)         
            .SetMaxNumberConnectionsPerWorker(1024)
            .SetSlabSizes(32 * 1024, 16 * 1024)
            .InjectRequestHandler(RequestHandler);
        
        var engine = builder.Build();
        engine.Run();
    }

    private static ValueTask RequestHandler(Connection connection)
    {
        var route = connection.BinaryH1HeaderData.Route.AsSpan();
        if (route[1] == (byte)'j') CommitJsonResponse(connection);
        else CommitPlainTextResponse(connection);
        return  ValueTask.CompletedTask;
    }
    
    [ThreadStatic] private static Utf8JsonWriter? t_utf8JsonWriter;
    private static readonly JsonContext SerializerContext = JsonContext.Default;
    private static unsafe void CommitJsonResponse(Connection connection)
    {
        var tail = connection.WriteBuffer.Tail;
        connection.WriteBuffer.WriteUnmanaged("HTTP/1.1 200 OK\r\n"u8 +
                                              "Content-Length:   \r\n"u8 +
                                              "Server: U\r\n"u8 +
                                              "Content-Type: application/json; charset=UTF-8\r\n"u8);
        connection.WriteBuffer.WriteUnmanaged(DateHelper.HeaderBytes);
        
        t_utf8JsonWriter ??= new Utf8JsonWriter(connection.WriteBuffer, new JsonWriterOptions { SkipValidation = true });
        t_utf8JsonWriter.Reset(connection.WriteBuffer);
        
        // Creating(Allocating) a new JsonMessage every request
        var message = new JsonMessage { Message = "Hello, World!" };
        // Serializing it every request
        JsonSerializer.Serialize(t_utf8JsonWriter, message, SerializerContext.JsonMessage);
        
        var contentLength = (int)t_utf8JsonWriter.BytesCommitted;
        
        byte* dst = connection.WriteBuffer.Ptr + tail + 33;
        int tens = contentLength / 10;
        int ones = contentLength - tens * 10;

        dst[0] = (byte)('0' + tens);
        dst[1] = (byte)('0' + ones);
        
    }

    private static ReadOnlySpan<byte> s_plainTextBody => "Hello, World!"u8;
    
    private static unsafe void CommitPlainTextResponse(Connection connection)
    {
        connection.WriteBuffer.WriteUnmanaged("HTTP/1.1 200 OK\r\n"u8 +
                                              "Content-Length: 13\r\n"u8 +
                                              "Server: U\r\n"u8 +
                                              "Content-Type: text/plain\r\n"u8);
        connection.WriteBuffer.WriteUnmanaged(DateHelper.HeaderBytes);
        connection.WriteBuffer.WriteUnmanaged(s_plainTextBody);
    }
}