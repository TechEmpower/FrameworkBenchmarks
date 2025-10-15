using System.IO.Pipelines;
using System.Text.Json;
using System.Text.Json.Serialization;
using Wired.IO.App;

namespace Fullstack;

internal class Program
{
    public static async Task Main(string[] args)
    {
        var expressBuilder = WiredApp.CreateExpressBuilder();

        await expressBuilder
            .Port(8080)
            .MapGet("/json", scope => async ctx =>
            {
                var payload = new JsonMessage { Message = JsonBody };
                var myHandler = CreateBoundHandler(ctx.Writer, payload);

                ctx
                    .Respond()
                    .Type("application/json"u8)
                    .Content(myHandler, 27);

            })
            .MapGet("/plaintext", scope => async ctx =>
            {
                ctx
                    .Respond()
                    .Type("text/plain"u8)
                    .Content(_plainTextBody);
            })
            .Build()
            .RunAsync();
    }
    
    private static ReadOnlySpan<byte> _plainTextBody => "Hello, World!"u8;
    private const string JsonBody = "Hello, World!";
    
    [ThreadStatic]
    private static Utf8JsonWriter? t_writer;
    private static readonly Action<PipeWriter, JsonMessage> StaticHandler = HandleFast;
    private static Action CreateBoundHandler(PipeWriter writer, JsonMessage message) => () => StaticHandler.Invoke(writer, message);
    private static void HandleFast(PipeWriter writer, JsonMessage message)
    {
        var utf8JsonWriter = t_writer ??= new Utf8JsonWriter(writer, new JsonWriterOptions { SkipValidation = true });
        utf8JsonWriter.Reset(writer);
        JsonSerializer.Serialize(utf8JsonWriter, message, SerializerContext.JsonMessage);
    }
    
    private static readonly JsonContext SerializerContext = JsonContext.Default;
}

public struct JsonMessage { public string Message { get; set; } }

[JsonSourceGenerationOptions(GenerationMode = JsonSourceGenerationMode.Serialization | JsonSourceGenerationMode.Metadata)]
[JsonSerializable(typeof(JsonMessage))]
public partial class JsonContext : JsonSerializerContext { }