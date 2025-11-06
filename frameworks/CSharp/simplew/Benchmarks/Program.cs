using System;
using System.Buffers.Text;
using System.Net;
using System.Threading.Tasks;
using SimpleW;

namespace Benchmarks;

internal static class Program
{
    public static async Task Main(string[] args)
    {
        var server = new SimpleWServer(IPAddress.Any, 8080);
        server.AddDynamicContent("/api");
        server.Start();
        
        await Task.Delay(-1);
    }
    
    public class BenchmarksController : Controller {
        [Route("GET", "/json")]
        public object Json() {
            return Response.MakeResponse(
                new { message = "Hello, World!" }, // object will be serialized
                addHeaders: new Dictionary<string, string>()
                {
                    { "Server", "SimpleW" },
                    { "Date", DateTime.Now.ToString("R") }
                }
                // compress parameter is default to null, so no compression
            );
        }

        [Route("GET", "/plaintext")]
        public object Plaintext() {
            return Response.MakeResponse(
                "Hello, World!",
                "text/plain",
                addHeaders: new Dictionary<string, string>()
                {
                    { "Server", "SimpleW" },
                    { "Date", DateTime.Now.ToString("R") }
                }
                // compress parameter is default to null, so no compression
            );
        }
    }
}