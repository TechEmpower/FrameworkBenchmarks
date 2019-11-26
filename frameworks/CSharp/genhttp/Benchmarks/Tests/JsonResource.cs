using System;
using System.Collections.Generic;
using System.Text;

using Newtonsoft.Json;

using GenHTTP.Modules.Webservices;

namespace Benchmarks.Tests
{

    public class JsonResult
    {

        [JsonProperty("message")]
        public string? Message { get; set; }

    }

    public class JsonResource
    {

        [Method]
        public JsonResult GetMessage() => new JsonResult() { Message = "Hello, World!" };

    }

}
