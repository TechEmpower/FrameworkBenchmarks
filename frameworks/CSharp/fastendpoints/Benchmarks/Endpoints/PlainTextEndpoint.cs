namespace Benchmarks.Endpoints;

sealed class PlainTextEndpoint : Ep.NoReq.Res<byte[]>
{
    static readonly byte[] _payload = "Hello, World!"u8.ToArray();

    public override void Configure()
    {
        Get("/plaintext");
        AllowAnonymous();
    }

    public override Task HandleAsync(CancellationToken ct)
    {
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        HttpContext.Response.ContentType = "text/plain";
        HttpContext.Response.ContentLength = _payload.Length;

        return HttpContext.Response.Body.WriteAsync(_payload, 0, _payload.Length);
    }
}