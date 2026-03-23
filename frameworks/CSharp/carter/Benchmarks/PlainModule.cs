using System.Text;
using Carter;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Routing;

namespace Benchmarks
{
    public class PlainModule : ICarterModule
    {
        private static readonly byte[] _helloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");


        public void AddRoutes(IEndpointRouteBuilder app)
        {
            app.MapGet("/plaintext", (HttpResponse res) =>
            {
                var payloadLength = _helloWorldPayload.Length;
                res.StatusCode = 200;
                res.ContentType = "text/plain";
                res.ContentLength = payloadLength;
                return res.Body.WriteAsync(_helloWorldPayload, 0, payloadLength);
            });
        }
    }
}