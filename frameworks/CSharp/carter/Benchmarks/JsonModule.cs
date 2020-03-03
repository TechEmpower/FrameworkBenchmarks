namespace Benchmarks
{
    using Carter;
    using System.Threading.Tasks;
    using Utf8Json;

    public class JsonModule : CarterModule
    {
        private const int _bufferSize = 27;

        public JsonModule() : base("json")
        {
            Get("/", (req, res) =>
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
}
