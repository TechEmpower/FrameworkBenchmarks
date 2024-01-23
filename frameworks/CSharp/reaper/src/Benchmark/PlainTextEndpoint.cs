using Reaper;
using Reaper.Attributes;

namespace Benchmark;

[ReaperRoute(HttpVerbs.Get, "/plaintext")]
public class PlainTextEndpoint : ReaperEndpointXR<string>
{
    public override Task<string> HandleAsync()
    {
        Context.Response.StatusCode = 200;
        Context.Response.ContentType = "text/plain";
        Context.Response.ContentLength = 13;
        return Task.FromResult("Hello, World!");
    }
}