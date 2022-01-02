using GenHTTP.Modules.Webservices;

namespace Benchmarks.Tests
{

    public sealed class JsonResult
    {

        public string Message { get; set; }

    }

    public sealed class JsonResource
    {

        [ResourceMethod]
        public JsonResult GetMessage() => new() { Message = "Hello, World!" };

    }

}
