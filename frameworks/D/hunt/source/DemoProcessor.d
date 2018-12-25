module DemoProcessor;

import hunt.io;
import http.Processor;
import stdx.data.json;

class DemoProcessor : HttpProcessor {
    this(TcpStream client) {
        super(client);
    }

    override void onComplete(HttpRequest req) {
        switch (req.uri) {
        case "/plaintext":
            respondWith("Hello, World!", 200, HttpHeader("Content-Type", "text/plain"));
            break;

        case "/json":
            JSONValue js = JSONValue(["message" : JSONValue("Hello, World!")]);
            respondWith(js.toJSON(), 200, HttpHeader("Content-Type", "application/json"));
            break;

        default:
            respondWith("The accessable path are: /plaintext and /json", 404);
            break;
        }
    }
}
