namespace Benchmarks.Endpoints;

public class PlainTextEndpoint : Endpoint<EmptyRequest, EmptyResponse>
{
    private static readonly byte[] payload = System.Text.Encoding.UTF8.GetBytes("Hello, World!");

    public override void Configure()
    {
        Get("/plaintext");
        AllowAnonymous();
    }

    public override Task HandleAsync(EmptyRequest _, CancellationToken __)
    {
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        HttpContext.Response.ContentType = "text/plain";
        HttpContext.Response.ContentLength = payload.Length;
        return HttpContext.Response.Body.WriteAsync(payload, 0, payload.Length);
    }
}