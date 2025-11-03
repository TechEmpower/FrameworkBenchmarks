import std.stdio;
import std.socket;
import std.array;
import std.algorithm;
import std.conv;
import std.ascii;
import core.stdc.stdlib;
import core.stdc.string;

import mir.ser;
import mir.ser.json;

import core.time;

import std.range.primitives;

import glow.xbuf;

import photon, photon.http, photon.mustache;

import mir.random : unpredictableSeedOf;
import mir.random.variable : UniformVariable;
import mir.random.engine.xorshift : Xorshift;

import dpq2;
import dpq2.conv.to_d_types;

struct Message {
    string message;
}

struct WorldResponse {
	int id;
	int randomNumber;
}

struct FortuneResponse {
	int id;
	string message;
}

enum connectionInfo = "host=tfb-database port=5432 "
						~ "dbname=hello_world  user=benchmarkdbuser password=benchmarkdbpass";
enum worldSize = 10000;
enum poolSize = 64;

shared Pool!Connection connectionPool;

class BenchmarkProcessor : HttpProcessor {
    HttpHeader[] plainTextHeaders = [HttpHeader("Content-Type", "text/plain; charset=utf-8")];
    HttpHeader[] jsonHeaders = [HttpHeader("Content-Type", "application/json")];
    HttpHeader[] htmlHeaders = [HttpHeader("Content-Type", "text/html; charset=utf-8")];
    Buffer!char outBuf;
    Buffer!WorldResponse worlds;
    UniformVariable!uint uniformVariable;
    Xorshift gen;

    this(Socket sock) {
		super(sock);
        gen = Xorshift(unpredictableSeed!uint);
		uniformVariable = UniformVariable!uint(1, worldSize);
		outBuf = Buffer!char(256);
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
        } else if(req.uri == "/fortunes") {
            fortunes();
        }
        else {
			respondWith("Not found", HttpStatus.NotFound, plainTextHeaders);
		}
    }

    final void plaintext() {
        respondWith("Hello, world!", HttpStatus.OK, plainTextHeaders);
    }

    final void json() {
        outBuf.clear();
        serializeJsonPretty!""(outBuf, Message("Hello, World!"));
        respondWith(outBuf.data, HttpStatus.OK, jsonHeaders);
    }

    final void db() {
        outBuf.clear();
        int id = uniformVariable(gen);
        auto c = connectionPool.acquire();
        scope(exit) connectionPool.release(c);
        QueryParams qp;
		qp.preparedStatementName("db_prpq");
		qp.argsVariadic(id);
		immutable result = c.execPrepared(qp).rangify.front;
		auto w = WorldResponse(id, result[0].as!PGinteger);
        serializeJsonPretty!""(outBuf, w);
        respondWith(outBuf.data, HttpStatus.OK, jsonHeaders);
    }

    // GET /queries?queries=...
    final void queries(const(char)[] uri) {
        outBuf.clear();
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
        serializeJsonPretty!""(outBuf, worlds.data);
        respondWith(outBuf.data, HttpStatus.OK, jsonHeaders);
    }
    
    // GET /updates?queries=...
    final void updates(const(char)[] uri) {
        outBuf.clear();
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
        serializeJsonPretty!""(outBuf, worlds.data);
        respondWith(outBuf.data, HttpStatus.OK, jsonHeaders);
    }

    final void fortunes() {
        outBuf.clear();
        auto c = connectionPool.acquire();
        scope(exit) connectionPool.release(c);
        import std.algorithm : map, sort;

		auto buf = Buffer!FortuneResponse(20);
		QueryParams qp;
		qp.preparedStatementName("fortune_prpq");
		auto result = c.execPrepared(qp).rangify;
		foreach (ref f; result) {
            buf.put(FortuneResponse(f[0].as!PGinteger, f[1].data.alloced));
        }
		buf.put(FortuneResponse(0, "Additional fortune added at request time"));
        auto data = buf.data;
		data.sort!((a, b) => a.message < b.message);
		mustache!(import("template.mustache"))(data, outBuf);
        foreach (ref v; data) {
            if (v.id != 0) dealloc(v.message);
        }
        respondWith(outBuf.data, HttpStatus.OK, htmlHeaders);
    }
}

string alloced(const(ubyte)[] data) {
    void* ptr = malloc(data.length);
    memcpy(ptr, data.ptr, data.length);
    return (cast(immutable(char)*)ptr)[0..data.length];
}

void dealloc(const(char)[] slice) {
    free(cast(void*)slice.ptr);
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
        c.prepareEx("fortune_prpq", "SELECT id, message::text FROM Fortune");
        c.prepareEx("db_prpq", "SELECT randomNumber, id FROM world WHERE id = $1");
        c.prepareEx("db_update_prpq", "UPDATE world SET randomNumber = $1  WHERE id = $2");
        return c;
    }, (ref Connection c) {
        destroy(c);
    });
    go(() => server());
    runScheduler();
}
