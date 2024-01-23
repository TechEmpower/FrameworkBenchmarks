namespace Benchmarks.Endpoints;

public sealed class JsonEndpoint : Endpoint<EmptyRequest, object>
{
    public override void Configure()
    {
        Get("/json");
        AllowAnonymous();
    }

    public override Task HandleAsync(EmptyRequest _, CancellationToken __)
    {
        HttpContext.Response.ContentLength = 27;
        return SendAsync(new { message = "Hello, World!" });
    }
}