namespace Benchmarks.Endpoints;

sealed class JsonEndpoint : Ep.NoReq.Res<object>
{
    public override void Configure()
    {
        Get("/json");
        AllowAnonymous();
    }

    public override Task HandleAsync(CancellationToken ct)
    {
        HttpContext.Response.ContentLength = 27;

        return SendAsync(new { message = "Hello, World!" });
    }
}