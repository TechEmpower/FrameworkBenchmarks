namespace Benchmarks.Endpoints;

public class Response
{
    public string message => "Hello, World!";
}

public class JsonEndpoint : Endpoint<EmptyRequest, Response>
{
    public override void Configure()
    {
        Get("/json");
        AllowAnonymous();
    }

    public override Task HandleAsync(EmptyRequest r, CancellationToken ct)
    {
        HttpContext.Response.ContentLength = 27;
        return SendAsync(Response);
    }
}