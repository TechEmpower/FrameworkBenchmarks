namespace Benchmarks
{
    using Carter;
    using System.Text;

    public class PlainModule : CarterModule
    {
        private static readonly byte[] _helloWorldPayload = Encoding.UTF8.GetBytes("Hello, World!");

        public PlainModule() : base("plaintext")
        {
            Get("/", (req, res) =>
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
