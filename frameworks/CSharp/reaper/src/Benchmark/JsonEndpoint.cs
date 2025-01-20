using Reaper;
using Reaper.Attributes;

namespace Benchmark;

public class JsonResponse
{
    public string Message { get; set; } = default!;
}

[ReaperRoute(HttpVerbs.Get, "/json")]
public class JsonEndpoint : ReaperEndpointXR<JsonResponse>
{
    public override Task ExecuteAsync()
    {
        Context.Response.ContentLength = 27;
        Result = new JsonResponse { Message = "Hello, World!" };
        return Task.CompletedTask;
    }
}