using System;
using Newtonsoft.Json;
using Tetsu.Web;

namespace Benchmark {
    public class Program {
        public static void Main() {
            var server = new Server();

            server.Handle("/plaintext", ctx =>
                ctx.Response.TextContent = "Hello, World!");

            server.Handle("/json", ctx => {
                ctx.Response.SetHeader("Content-Type", "application/json");
                ctx.Response.TextContent = JsonConvert.SerializeObject(new {
                    message = "Hello, World!"
                });
            });

            server.AddMiddleware(ctx =>
                ctx.Response.SetHeader("Date", DateTime.UtcNow.ToString("r")));

            server.Listen("0.0.0.0", 1234).Wait();
        }
    }
}
