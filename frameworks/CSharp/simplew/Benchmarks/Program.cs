using System;
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
            return new { message = "Hello, World !" };
        }
        
        [Route("GET", "/plaintext")]
        public object Plaintext() {
            return "Hello, World !" ;
        }
    }
}