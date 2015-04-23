namespace NancyModules
{
    using Nancy;

    public class JsonModule : NancyModule
    {
        public JsonModule() : base("/json")
        {
            Get["/"] = _ => Response.AsJson(new { message = "Hello, World!" });
        }
    }
}
