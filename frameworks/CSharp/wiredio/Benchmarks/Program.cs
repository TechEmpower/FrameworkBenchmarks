using System.Buffers;
using System.Net;
using System.Text.Json;
using Wired.IO.App;
using Wired.IO.Http11.Response.Content;
using Wired.IO.Protocol.Response;

var builder = WiredApp.CreateBuilder();

await builder
    .Endpoint(IPAddress.Any, 8080)
    .MapGet("/plaintext", scope => async context =>
    {
        context
            .Writer.Write("HTTP/1.1 200 OK\r\n"u8);
        context
            .Writer.Write("Content-Length:12\r\n"u8);
        context
            .Writer.Write("Content-Type: text/plain\r\nConnection: keep-alive\r\n\r\n"u8);
        context
            .Writer.Write("Hello World!\r\n"u8);

        await context.Writer.FlushAsync();
    })
    .MapGet("/json", scope => context =>
    {
        context
            .Respond()
            .Status(ResponseStatus.Ok)
            .Content(new JsonContent(new
            {
                Message = "Hello World!"
            }, JsonSerializerOptions.Default))
            .Type("application/json");
    })
    .Build()
    .RunAsync();