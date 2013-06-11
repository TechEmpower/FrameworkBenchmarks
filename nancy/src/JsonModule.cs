using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Nancy;

namespace NancyBenchmark
{
    public class JsonModule : NancyModule
    {
        public JsonModule() : base("/json")
        {
            Get["/"] = x =>
            {
                return Response.AsJson(new { message = "Hello, World!" });
            };
        }
    }
}
