import std.stdio;
import std.socket;
import std.array;

import mir.ser;
import mir.ser.json;

import std.range.primitives;

import glow.xbuf;

import photon, photon.http;

struct Message
{
    string message;
}

class BenchmarkProcessor : HttpProcessor {
    HttpHeader[] plainText = [HttpHeader("Content-Type", "text/plain; charset=utf-8")];
    HttpHeader[] json = [HttpHeader("Content-Type", "application/json")];
    Buffer!char jsonBuf;
    this(Socket sock) {
		super(sock);
		jsonBuf = Buffer!char(256);
	}

    override void handle(HttpRequest req) {
		if (req.uri == "/plaintext") {
			respondWith("Hello, world!", HttpStatus.OK, plainText);
		} else if(req.uri == "/json") {
			jsonBuf.clear();
			serializeJsonPretty!""(jsonBuf, Message("Hello, World!"));
			respondWith(jsonBuf.data, HttpStatus.OK, json);
		} else {
			respondWith("Not found", HttpStatus.NotFound, plainText);
		}
    }
}

void server_worker(Socket client) {
    scope processor =  new BenchmarkProcessor(client);
    try {
        processor.run();
    }
    catch(Exception e) {
        debug stderr.writeln(e);
    }
}

void server() {
    Socket server = new TcpSocket();
    server.setOption(SocketOptionLevel.SOCKET, SocketOption.REUSEADDR, true);
    server.bind(new InternetAddress("0.0.0.0", 8080));
    server.listen(1000);

    debug writeln("Started server");

    void processClient(Socket client) {
        go(() => server_worker(client));
    }

    while(true) {
        try {
            debug writeln("Waiting for server.accept()");
            Socket client = server.accept();
            debug writeln("New client accepted");
            processClient(client);
        }
        catch(Exception e) {
            writefln("Failure to accept %s", e);
        }
    }
}

void main() {
    initPhoton();
    go(() => server());
    runScheduler();
}
