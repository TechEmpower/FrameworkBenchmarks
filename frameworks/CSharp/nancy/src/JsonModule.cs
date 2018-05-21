namespace Nancy.Benchmark
{
    public class JsonModule : NancyModule
    {
        public JsonModule() : base("/json")
        {
            Get("/", args =>
            {
                return Response.AsJson(new { message = "Hello, World!" });
            });
        }
    }
}
