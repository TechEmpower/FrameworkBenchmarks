using Nancy;

namespace NancyModules
{
    public class PlaintextModule : NancyModule
    {
        public PlaintextModule() : base("/plaintext")
        {
            Get["/"] = _ => Response.AsText("Hello, World!");
        }
    }
}