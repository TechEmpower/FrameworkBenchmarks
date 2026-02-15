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
        SimpleWServer server = new(IPAddress.Any, 8080);
        
        // minimal api
        server.MapGet("/json", (HttpSession session) => {
            return session.Response
                          .Json(new { message = "Hello, World!" })
                          .AddHeader("Server", "SimpleW")
                          .AddHeader("Date", DateTime.Now.ToString("R"))
                          .NoCompression();
        });
        server.MapGet("/plaintext", (HttpSession session) => {
            return session.Response
                      .Text("Hello, World!")
                      .AddHeader("Server", "SimpleW")
                      .AddHeader("Date", DateTime.Now.ToString("R"))
                      .NoCompression();
        });
        
        await server.RunAsync();
    }

}
