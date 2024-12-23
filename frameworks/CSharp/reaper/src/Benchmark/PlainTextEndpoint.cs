using Reaper;
using Reaper.Attributes;

namespace Benchmark;

[ReaperRoute(HttpVerbs.Get, "/plaintext")]
public class PlainTextEndpoint : ReaperEndpointXR<string>
{
    public override Task ExecuteAsync()
    {
        Context.Response.StatusCode = 200;
        Context.Response.ContentType = "text/plain";
        Context.Response.ContentLength = 13;
        Result = "Hello, World!";
        return Task.CompletedTask;
    }
}