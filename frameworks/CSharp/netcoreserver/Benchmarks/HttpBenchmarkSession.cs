using System;
using System.Net.Sockets;
using System.Text.Json;

using NetCoreServer;

namespace Benchmarks
{

    public class HttpBenchmarkSession : HttpSession
    {

        public HttpBenchmarkSession(HttpServer server) : base(server) { }

        protected override void OnReceivedRequest(HttpRequest request)
        {
            if (request.Url.StartsWith("/plaintext"))
            {
                SendResponseAsync(MakeResponse("Hello, World!", "text/plain; charset=UTF-8"));
            }
            else if (request.Url.StartsWith("/json"))
            {
                var value = new JsonResult() { Message = "Hello, World!" };
                var serialized = JsonSerializer.Serialize(value);

                SendResponseAsync(MakeResponse(serialized, "application/json; charset=UTF-8"));
            }
            else
            {
                SendResponseAsync(Response.MakeErrorResponse("Not found", 404));
            }
        }

        protected override void OnReceivedRequestError(HttpRequest request, string error)
        {
            Console.WriteLine($"Request error: {error}");
        }

        protected override void OnError(SocketError error)
        {
            Console.WriteLine($"HTTP session caught an error: {error}");
        }

        private static HttpResponse MakeResponse(string value, string contentType)
        {
            var response = new HttpResponse(200, "OK", "HTTP/1.1");

            response.SetHeader("Server", "NetCoreServer");
            response.SetHeader("Date", DateTime.UtcNow.ToString("r"));
            response.SetHeader("Content-Type", contentType);

            response.SetBody(value);

            return response;
        }

    }

}
