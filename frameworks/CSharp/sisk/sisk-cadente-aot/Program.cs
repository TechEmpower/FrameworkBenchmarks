using System.Net;
using System.Text;
using System.Text.Json;
using Sisk.Cadente;

var host = new HttpHost ( new IPEndPoint ( IPAddress.Any, 8080 ) );
host.ContextCreated += Host_ContextCreated;

host.Start ();
Thread.Sleep ( Timeout.Infinite );

void Host_ContextCreated ( HttpHost sender, HttpHostContext session ) {
    var request = session.Request;

    if (request.Path == "/plaintext") {
        SerializePlainTextResponse ( session.Response );
    }
    else if (request.Path == "/json") {
        SerializeJsonResponse ( session.Response );
    }
    else {
        session.Response.StatusCode = 404;
    }
}

static void SerializePlainTextResponse ( HttpHostContext.HttpResponse response ) {

    var messageBytes = Encoding.UTF8.GetBytes ( "Hello, world!" );

    response.Headers.Add ( new HttpHeader ( "Content-Type", "text/plain" ) );
    response.ResponseStream = new MemoryStream ( messageBytes );
}

static void SerializeJsonResponse ( HttpHostContext.HttpResponse response ) {

    var contentBytes = JsonSerializer.SerializeToUtf8Bytes ( new {
        message = "Hello, world!"
    } );

    response.Headers.Add ( new HttpHeader ( "Content-Type", "application/json" ) );
    response.ResponseStream = new MemoryStream ( contentBytes );
}