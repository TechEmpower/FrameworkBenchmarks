using System.Net;
using System.Text.Json;
using Wired.IO.App;
using Wired.IO.Http11.Response.Content;
using Wired.IO.Protocol.Response;
using StringContent = Wired.IO.Http11.Response.Content.StringContent;

var builder = WiredApp.CreateBuilder();

await builder
    .Endpoint(IPAddress.Any, 8080)
    .MapGet("/plaintext", scope => context =>
    {
        context
            .Respond()
            .Status(ResponseStatus.Ok)
            .Content(new StringContent("Hello, World!"))
            .Type("text/plain");
    })
    .MapGet("/json", scope => context =>
    {
        context
            .Respond()
            .Status(ResponseStatus.Ok)
            .Content(new JsonContent(new
            {
                Message = "Hello, World!"
            }, JsonSerializerOptions.Default))
            .Type("application/json");
    })
    .Build()
    .RunAsync();