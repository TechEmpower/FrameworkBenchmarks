// ReSharper disable always SuggestVarOrType_BuiltInTypes
// (var is avoided intentionally in this project so that concrete types are visible at call sites.)
// ReSharper disable always StackAllocInsideLoop

using System.Runtime.CompilerServices;
using System.Text.Json;
using Unhinged;

#pragma warning disable CA2014

/* (MDA2AV)Dev notes:
 * 
 * Wired.IO Platform benchmark using [Unhinged - https://github.com/MDA2AV/Unhinged] epoll engine.
 *
 * This test was created purely for benchmark/comparison between .NET solutions.
 * It should not be considered EVER as a go-to framework to build any kind of webserver!
 * For such purpose please use the main Wired.IO framework [Wired.IO - https://github.com/MDA2AV/Wired.IO].
 *
 * This benchmarks follows the JsonSerialization and PlainText rules imposed by the TechEmpower team.
 *
 * The Http parsing by the Unhinged engine is still naive(work in progress), yet it's development will not have any impact
 * on these benchmarks results as the extra request parsing overhead is much smaller than the read/send syscalls'.
 */

namespace Platform;

[SkipLocalsInit]
internal static class Program
{
    public static void Main(string[] args)
    {
        var builder = UnhingedEngine
            .CreateBuilder()
            .SetPort(8080)
            
            .SetNWorkersSolver(() => Environment.ProcessorCount )  
            
            // Accept up to 16384 connections
            .SetBacklog(16384) 
            
            // Max 512 epoll events per wake (quite overkill)
            .SetMaxEventsPerWake(512)         
            
            // Max 1024 connection per thread
            .SetMaxNumberConnectionsPerWorker(1024)
            
            // 32KB in and 16KB out slabs to handle 16 pipeline depth
            .SetSlabSizes(32 * 1024, 16 * 1024)
            .InjectRequestHandler(RequestHandler);
        
        var engine = builder.Build();
        engine.Run();
    }

    private const string Json = "/json";
    private const string PlainText = "/plaintext";

    private static ValueTask RequestHandler(Connection connection)
    {
        // FNV-1a Hashed routes to avoid string allocations
        if(connection.H1HeaderData.Route == Json)          // /json
            CommitJsonResponse(connection);
       
        else if (connection.H1HeaderData.Route == PlainText)   // /plaintext
            CommitPlainTextResponse(connection);
        
        return  ValueTask.CompletedTask;
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