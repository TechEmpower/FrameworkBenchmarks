using System.Net.Http.Json;
using Sisk.Core.Http;
using Sisk.Core.Routing;

var app = HttpServer.CreateBuilder ( host => {
    host.UseListeningPort ( "http://+:8080/" );
} ).Build ();

app.Router.SetRoute ( RouteMethod.Get, "/plaintext", PlainText );
app.Router.SetRoute ( RouteMethod.Get, "/json", Json );

app.Start ();

static HttpResponse PlainText ( HttpRequest request ) {
    return new HttpResponse ( "Hello, world!" );
}

static HttpResponse Json ( HttpRequest request ) {
    return new HttpResponse ( JsonContent.Create ( new {
        message = "Hello, world!"
    } ) );
}