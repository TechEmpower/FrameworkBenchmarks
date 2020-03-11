using GenHTTP.Modules.Webservices;

namespace Benchmarks.Tests
{

    public class JsonResult
    {

        public string? Message { get; set; }

    }

    public class JsonResource
    {

        [Method]
        public JsonResult GetMessage() => new JsonResult() { Message = "Hello, World!" };

    }

}
