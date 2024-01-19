using System.Text.Json.Serialization;
using Benchmark;
using Reaper;

var builder = WebApplication.CreateSlimBuilder(args);
builder.Logging.ClearProviders();
builder.Logging.Configure(o => o.ActivityTrackingOptions = ActivityTrackingOptions.None);
builder.Services.ConfigureHttpJsonOptions(o =>
{
    o.SerializerOptions.TypeInfoResolverChain.Insert(0, SourceGenerationContext.Default);
});
builder.UseReaper();

var app = builder.Build();

app.UseReaperMiddleware();
app.MapReaperEndpoints();

app.Run();

[JsonSerializable(typeof(JsonResponse))]
internal partial class SourceGenerationContext : JsonSerializerContext { }