import std.stdio;
import std.socket;
import std.array;
import std.algorithm;
import std.conv;
import std.ascii;

import mir.ser;
import mir.ser.json;

import core.time;

import std.range.primitives;

import glow.xbuf;

import photon, photon.http;

import mir.random : unpredictableSeedOf;
import mir.random.variable : UniformVariable;
import mir.random.engine.xorshift : Xorshift;

import dpq2;

struct Message {
    string message;
}

struct WorldResponse {
	int id;
	int randomNumber;
}

enum connectionInfo = "host=tfb-database port=5432 "
						~ "dbname=hello_world  user=benchmarkdbuser password=benchmarkdbpass";
enum worldSize = 10000;
enum poolSize = 64;

shared Pool!Connection connectionPool;

class BenchmarkProcessor : HttpProcessor {
    HttpHeader[] plainTextHeaders = [HttpHeader("Content-Type", "text/plain; charset=utf-8")];
    HttpHeader[] jsonHeaders = [HttpHeader("Content-Type", "application/json")];
    Buffer!char jsonBuf;
    Buffer!WorldResponse worlds;
    UniformVariable!uint uniformVariable;
    Xorshift gen;

    this(Socket sock) {
		super(sock);
        gen = Xorshift(unpredictableSeed!uint);
		uniformVariable = UniformVariable!uint(1, worldSize);
		jsonBuf = Buffer!char(256);
        worlds = Buffer!WorldResponse(500);
	}

    override void handle(HttpRequest req) {
		if (req.uri == "/plaintext") {
			plaintext();
		} else if(req.uri == "/json") {
			json();
		} else if(req.uri == "/db") {
            db();
        } else if(req.uri.startsWith("/queries")) {
            queries(req.uri);
        } else if(req.uri.startsWith("/updates")) {
            updates(req.uri);
        }
        else {
			respondWith("Not found", HttpStatus.NotFound, plainTextHeaders);
		}
    }

    final void plaintext() {
        respondWith("Hello, world!", HttpStatus.OK, plainTextHeaders);
    }

    final void json() {
        jsonBuf.clear();
        serializeJsonPretty!""(jsonBuf, Message("Hello, World!"));
        respondWith(jsonBuf.data, HttpStatus.OK, jsonHeaders);
    }

    final void db() {
        jsonBuf.clear();
        int id = uniformVariable(gen);
        auto c = connectionPool.acquire();
        scope(exit) connectionPool.release(c);
        QueryParams qp;
		qp.preparedStatementName("db_prpq");
		qp.argsVariadic(id);
		immutable result = c.execPrepared(qp).rangify.front;
		auto w = WorldResponse(id, result[0].as!PGinteger);
        serializeJsonPretty!""(jsonBuf, w);
        respondWith(jsonBuf.data, HttpStatus.OK, jsonHeaders);
    }

    // GET /queries?queries=...
    final void queries(const(char)[] uri) {
        jsonBuf.clear();
        worlds.clear();
        auto c = connectionPool.acquire();
        scope(exit) connectionPool.release(c);

		int count = 1;
        auto i = uri.indexOf("?queries=");
        if (i > 0 && i + 9 <= uri.length && isDigit(uri[i+9]))
		{
			try count = min(max(uri[i+9..$].to!int, 1), 500);
			catch (ConvException) {}
		}

		QueryParams qp;
		qp.preparedStatementName("db_prpq");
		foreach (_; 0..count) {
			int id = uniformVariable(gen);
			qp.argsVariadic(id);
			immutable result = c.execPrepared(qp).rangify.front;
			worlds.put(WorldResponse(id, result[0].as!PGinteger));
		}
        serializeJsonPretty!""(jsonBuf, worlds.data);
        respondWith(jsonBuf.data, HttpStatus.OK, jsonHeaders);
    }
    
    // GET /updates?queries=...
    final void updates(const(char)[] uri) {
        jsonBuf.clear();
        worlds.clear();
        auto c = connectionPool.acquire();
        scope(exit) connectionPool.release(c);

		int count = 1;
        auto i = uri.indexOf("?queries=");
        if (i > 0 && i + 9 <= uri.length && isDigit(uri[i+9]))
		{
			try count = min(max(uri[i+9..$].to!int, 1), 500);
			catch (ConvException) {}
		}

		QueryParams qp;
		qp.preparedStatementName("db_prpq");

		QueryParams qp_update;
		qp_update.preparedStatementName("db_update_prpq");

		foreach (_; 0..count) {
			int id = uniformVariable(gen);
			qp.argsVariadic(id);
			immutable result = c.execPrepared(qp).rangify.front;
			auto w = WorldResponse(id, result[0].as!PGinteger);
            
            // update random number
			w.randomNumber = uniformVariable(gen);
			qp_update.argsVariadic(w.randomNumber, id);

			// persist to DB
			c.execPrepared(qp_update);
            worlds.put(w);
		}
        serializeJsonPretty!""(jsonBuf, worlds.data);
        respondWith(jsonBuf.data, HttpStatus.OK, jsonHeaders);
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
    connectionPool = pool(poolSize, 15.seconds, () {
        auto c = new Connection(connectionInfo);
        c.prepareEx("db_prpq", "SELECT randomNumber, id FROM world WHERE id = $1");
        c.prepareEx("db_update_prpq", "UPDATE world SET randomNumber = $1  WHERE id = $2");
        return c;
    }, (ref Connection c) {
        destroy(c);
    });
    go(() => server());
    runScheduler();
}
