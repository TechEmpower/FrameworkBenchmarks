using System.Net.Http.Json;
using System.Text;
using Sisk.Core.Http;
using Sisk.Core.Routing;

var app = HttpServer.CreateBuilder ( host => {
    host.UseListeningPort ( "http://+:8080/" );
    host.UseConfiguration ( config => {
        config.AccessLogsStream = null;
    } );
} ).Build ();

app.Router.SetRoute ( RouteMethod.Get, "/plaintext", PlainText );
app.Router.SetRoute ( RouteMethod.Get, "/json", Json );

app.Start ();

static HttpResponse PlainText ( HttpRequest request ) {
    return new HttpResponse ( new StringContent ( "Hello, World!", Encoding.UTF8, "text/plain" ) );
}

static HttpResponse Json ( HttpRequest request ) {
    return new HttpResponse ( JsonContent.Create ( new {
        message = "Hello, World!"
    } ) );
}