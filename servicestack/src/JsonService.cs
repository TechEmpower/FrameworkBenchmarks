using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ServiceStack.ServiceHost;
using ServiceStack.ServiceInterface;

namespace ServiceStackBenchmark
{
    [Route("/json")]
    public class Json
    {
        public string message { get; set; }
    }

    public class JsonService : Service
    {
        public Json Get(Json empty)
        {
            return new Json { message = "Hello, World!" };
        }
    }
}
