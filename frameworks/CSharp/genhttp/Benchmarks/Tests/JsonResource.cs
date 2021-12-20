using GenHTTP.Modules.Webservices;

namespace Benchmarks.Tests
{

    public sealed class JsonResult
    {

        public string Message { get; set; }

    }

    public sealed class JsonResource
    {
        private static readonly JsonResult _Result = new() { Message = "Hello, World!" };

        [ResourceMethod]
        public JsonResult GetMessage() => _Result;

    }

}
