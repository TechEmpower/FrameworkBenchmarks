using Carter;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Routing;
using Utf8Json;

namespace Benchmarks
{
    public class JsonModule : ICarterModule
    {
        private const int _bufferSize = 27;

        public struct JsonMessage
        {
            public string message;
        }

        public void AddRoutes(IEndpointRouteBuilder app)
        {
            app.MapGet("/json", (HttpResponse res) =>
            {
                res.StatusCode = 200;
                res.ContentType = "application/json";
                res.ContentLength = _bufferSize;

                var msg = new JsonMessage { message = "Hello, World!" };

                return JsonSerializer.SerializeAsync(res.Body, msg);
            });
        }
    }
}