import std.stdio;
import std.socket;
import std.array;

import asdf.serialization;

import photon, photon.http;

struct Message
{
    string message;
}

class BenchmarkProcessor : HttpProcessor {
    HttpHeader[] plainText = [HttpHeader("Content-Type", "text/plain; charset=utf-8")];
    HttpHeader[] json = [HttpHeader("Content-Type", "application/json")];
    Appender!(char[]) jsonBuf;
    this(Socket sock) {
		super(sock);
		jsonBuf = appender!(char[]);
	}

    override void handle(HttpRequest req) {
		if (req.uri == "/plaintext") {
			respondWith("Hello, world!", 200, plainText);
		} else if(req.uri == "/json") {
			jsonBuf.clear();
			Message("Hello, World!").serializeToJsonPretty!""(jsonBuf);
			respondWith(jsonBuf.data, 200, json);
		} else {
			respondWith("Not found", 404, plainText);
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
    startloop();
    go(() => server());
    runFibers();
}
