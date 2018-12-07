module DemoProcessor;

import hunt.io;
import http.Processor;
import std.json;

class DemoProcessor : HttpProcessor {
    this(TcpStream client) {
        super(client);
    }

    override void onComplete(HttpRequest req) {
        switch (req.uri) {
        case "/plaintext":
            respondWith("Hello, World!", 200);
            break;

        case "/json":
            JSONValue js;
            js["message"] = "Hello, World!";
            string content = js.toString();
            respondWith(content, 200, HttpHeader("Content-Type", "application/json"));
            break;

        default:
            respondWith("The accessable path are: /plaintext and /json", 404);
            break;
        }
    }
}
