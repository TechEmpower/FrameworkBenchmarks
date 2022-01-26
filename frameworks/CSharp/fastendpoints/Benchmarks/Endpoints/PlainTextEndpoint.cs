namespace Benchmarks.Endpoints;

public class PlainTextEndpoint : Endpoint<EmptyRequest, object>
{
    private static readonly byte[] payload = System.Text.Encoding.UTF8.GetBytes("Hello, World!");

    public override void Configure()
    {
        Get("/plaintext");
        AllowAnonymous();
    }

    public override Task HandleAsync(EmptyRequest r, CancellationToken ct)
    {
        return SendBytesAsync(payload, contentType: "text/plain");
    }
}