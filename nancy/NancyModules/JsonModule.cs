namespace NancyModules
{
    using Nancy;

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
