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
    public override Task<JsonResponse> HandleAsync()
    {
        Context.Response.ContentLength = 27;
        return Task.FromResult(new JsonResponse { Message = "Hello, World!" });
    }
}