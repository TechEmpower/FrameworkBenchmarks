using System.Text;
using System.Text.Json;
using Sisk.Cadente;

var host = new HttpHost ( 8080, session => {
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
} );

host.Start ();
Thread.Sleep ( Timeout.Infinite );

static void SerializePlainTextResponse ( HttpResponse response ) {
    var contentBytes = Encoding.UTF8.GetBytes ( "Hello, world!" );

    response.Headers.Add ( new HttpHeader ( "Content-Type", "text/plain" ) );
    response.ResponseStream = new MemoryStream ( contentBytes );
}

static void SerializeJsonResponse ( HttpResponse response ) {
    var contentBytes = JsonSerializer.SerializeToUtf8Bytes ( new {
        message = "Hello, world!"
    } );

    response.Headers.Add ( new HttpHeader ( "Content-Type", "application/json; charset=utf-8" ) );
    response.ResponseStream = new MemoryStream ( contentBytes );
}