using System.Text.Json;
using GenHTTP.Api.Protocol;
using GenHTTP.Modules.IO;
using GenHTTP.Modules.Layouting;
using GenHTTP.Modules.Layouting.Provider;
using Unhinged;
using Unhinged.GenHttp.Experimental;

internal class Program
{
    public static void Main(string[] args)
    {
        var builder = UnhingedEngine
            .CreateBuilder()
            .SetNWorkersSolver(() => Environment.ProcessorCount)
            .SetBacklog(16384)
            .SetMaxEventsPerWake(512)
            .SetMaxNumberConnectionsPerWorker(512)
            .SetPort(8080)
            .SetSlabSizes(32 * 1024, 16 * 1024)
            .Map(CreateLayoutBuilder());
        
        var engine = builder.Build();
        engine.Run();
    }

    private static LayoutBuilder CreateLayoutBuilder() =>
        Layout
            .Create()
            .Add("/plaintext", Content.From(Resource.FromString("Hello, World!")))

            .Add("/json", Content.From(
                Resource.FromString(JsonSerializer.Serialize(new JsonMessage { message = "Hello, World!" }))
                    .Type(new FlexibleContentType("application/json"))));
}

public class JsonMessage
{
    public string message { get; set; }
}