using GenHTTP.Modules.Webservices;

namespace Benchmarks.Tests
{

    public class JsonResult
    {

        public string Message { get; set; }

    }

    public class JsonResource
    {

        [ResourceMethod]
        public JsonResult GetMessage() => new JsonResult() { Message = "Hello, World!" };

    }

}
