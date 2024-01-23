using Sisk.Core.Http;
using Sisk.Core.Routing;
using System.Net.Http.Json;

var app = HttpServer.CreateBuilder(host =>
{
    host.UseListeningPort("http://+:8080/");
});

app.Router.SetRoute(RouteMethod.Get, "/plaintext", PlainText);
app.Router.SetRoute(RouteMethod.Get, "/json", Json);

app.Start();

static HttpResponse PlainText(HttpRequest request)
{
    return new HttpResponse().WithContent("Hello, world!");
}

static HttpResponse Json(HttpRequest request)
{
    return new HttpResponse().WithContent(JsonContent.Create(new
    {
        message = "Hello, world!"
    }));
}