namespace Benchmarks.Endpoints;

public class JsonEndpoint : Endpoint<object, object>
{
    public override void Configure()
    {
        Get("/json");
        AllowAnonymous();
    }

    public override Task HandleAsync(object _, CancellationToken __)
    {
        HttpContext.Response.ContentLength = 27;
        return SendAsync(new { message = "Hello, World!" });
    }
}