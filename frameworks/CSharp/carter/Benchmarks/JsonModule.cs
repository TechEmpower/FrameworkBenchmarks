namespace Benchmarks;

using Carter;

using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Routing;

using Utf8Json;

public class JsonModule : CarterModule
{
    private const int _bufferSize = 27;

    public JsonModule() : base("json")
    {
    }

    public override void AddRoutes(IEndpointRouteBuilder app)
    {
        app.MapGet("/", (HttpResponse res) =>
        {
            res.StatusCode = 200;
            res.ContentType = "application/json";
            res.ContentLength = _bufferSize;

            var msg = new JsonMessage { message = "Hello, World!" };

            return JsonSerializer.SerializeAsync(res.Body, msg);
        });
    }

    public struct JsonMessage
    {
        public string message;
    }
}
