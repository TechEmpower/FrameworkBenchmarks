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
            .SetNWorkersSolver(() => Environment.ProcessorCount - 2)    // Number of working threads, depends on a lot of things
            .SetBacklog(16384)                      // Accept up to 16384 connections
            .SetMaxEventsPerWake(512)               // Max 512 epoll events per wake (quite overkill)
            .SetMaxNumberConnectionsPerWorker(512)  // Max 512 connection per thread
            .SetPort(8080)
            .SetSlabSizes(32 * 1024, 32 * 1024) // 32KB slabs to handle high pipeline depth
            .InjectRequestHandler(RequestHandler);
        
        var engine = builder.Build();
        
        engine.Run();
    }

    private static void RequestHandler(Connection connection)
    {
        // FNV-1a Hashed routes to avoid string allocations
        if(connection.HashedRoute == 291830056)          // /json
            CommitJsonResponse(connection);
       
        else if (connection.HashedRoute == 3454831873)   // /plaintext
            CommitPlainTextResponse(connection);
    }
    
    [ThreadStatic] private static Utf8JsonWriter? t_utf8JsonWriter;
    private static readonly JsonContext SerializerContext = JsonContext.Default;
    private static void CommitJsonResponse(Connection connection)
    {
        connection.WriteBuffer.WriteUnmanaged("HTTP/1.1 200 OK\r\n"u8 +
                                              "Server: W\r\n"u8 +
                                              "Content-Type: application/json; charset=UTF-8\r\n"u8 +
                                              "Content-Length: 27\r\n"u8);
        connection.WriteBuffer.WriteUnmanaged(DateHelper.HeaderBytes);
        
        t_utf8JsonWriter ??= new Utf8JsonWriter(connection.WriteBuffer, new JsonWriterOptions { SkipValidation = true });
        t_utf8JsonWriter.Reset(connection.WriteBuffer);
        
        // Creating(Allocating) a new JsonMessage every request
        var message = new JsonMessage { Message = "Hello, World!" };
        // Serializing it every request
        JsonSerializer.Serialize(t_utf8JsonWriter, message, SerializerContext.JsonMessage);
    }

    private static void CommitPlainTextResponse(Connection connection)
    {
        connection.WriteBuffer.WriteUnmanaged("HTTP/1.1 200 OK\r\n"u8 +
                                              "Server: W\r\n"u8 +
                                              "Content-Type: text/plain\r\n"u8 +
                                              "Content-Length: 13\r\n"u8);
        connection.WriteBuffer.WriteUnmanaged(DateHelper.HeaderBytes);
        connection.WriteBuffer.WriteUnmanaged("Hello, World!"u8);
    }
}