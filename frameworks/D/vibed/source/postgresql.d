import vibe.core.core;
import vibe.http.router;
import vibe.http.server;
import vibe.web.web;
import vibe.db.postgresql;

import mir.random : unpredictableSeedOf;
import mir.random.variable : UniformVariable;
import mir.random.engine.xorshift : Xorshift;

import std.conv : ConvException, to;
import std.array;

enum worldSize = 10000;
enum poolSize = 256;

PostgresClient client;

shared static this()
{
	import derelict.pq.pq;
	import derelict.util.exception : ShouldThrow;
	ShouldThrow myMissingSymCB( string symbolName ) { return ShouldThrow.No; }
	DerelictPQ.missingSymbolCallback = &myMissingSymCB;
}

void main()
{
	runWorkerTaskDist(&runServer);
	runApplication();
}

void runServer()
{
	auto router = new URLRouter;
	router.registerWebInterface(new WebInterface);
	router.rebuild();

	auto settings = new HTTPServerSettings;
	settings.options |= HTTPServerOption.reusePort;
	settings.port = 8080;
	listenHTTP(settings, router);
}

class WebInterface {
	private {
		UniformVariable!uint _uniformVariable;
		Xorshift _gen;
	}

	this()
	{
		_gen = Xorshift(unpredictableSeedOf!uint);
		_uniformVariable = UniformVariable!uint(1, worldSize);
	}

	// GET /
	void get()
	{
		render!"index.dt";
	}

	// GET /json
	void getJson(HTTPServerResponse res)
	{
		// NOTE: the status and content type parameters are optional, but we need
		// to specify them, because the default content type is "application/json; charset=UTF8"
		res.writeJsonBody(Message("Hello, World!"), HTTPStatus.ok, "application/json");
	}

	// GET /db
	void getDB(HTTPServerResponse res)
	{
		auto conn = client.lockConnection();
		scope(exit)	delete conn;

		immutable id = _uniformVariable(_gen);
		immutable query = "SELECT randomNumber FROM world WHERE id = " ~  id.to!string;
		immutable result = conn.execStatement(query, ValueFormat.BINARY).rangify.front;
		auto w = WorldResponse(id, result[0].as!PGinteger);
		res.writeJsonBody(w, HTTPStatus.ok, "application/json");
	}

	// GET /queries?queries=...
	void getQueries(HTTPServerResponse res, string queries)
	{
		import std.algorithm : min, max;

		auto conn = client.lockConnection();
		scope(exit)	delete conn;

		// Convert the "queries" parameter to int and ignore any conversion errors
		// Note that you'd usually declare queries as int instead. However, the
		// test required to gracefully handle errors here.
		int count = 1;
		try count = min(max(queries.to!int, 1), 500);
		catch (ConvException) {}

		// assemble the response array
		scope data = new WorldResponse[count];
		foreach (ref w; data) {
			immutable id = _uniformVariable(_gen);
			immutable query = "SELECT randomNumber FROM world WHERE id = " ~  id.to!string;
			immutable result = conn.execStatement(query, ValueFormat.BINARY).rangify.front;
			w = WorldResponse(id, result[0].as!PGinteger);
		}

		// write response as JSON
		res.writeJsonBody(data, HTTPStatus.ok, "application/json");
	}

	// GET /fortunes
	void getFortunes()
	{
		import std.algorithm : map, sort;

		auto conn = client.lockConnection();
		scope(exit)	delete conn;

		FortuneResponse[] data;
		immutable query = "SELECT id, message::text FROM Fortune";
		auto result = conn.execStatement(query, ValueFormat.BINARY).rangify;
		data = result.map!(f => FortuneResponse(f[0].as!PGinteger, f[1].as!PGtext)).array;
		data ~= FortuneResponse(0, "Additional fortune added at request time.");
		data.sort!((a, b) => a.message < b.message);
		render!("fortunes.dt", data);
	}

	// GET /updates?queries=...
	void getUpdates(HTTPServerResponse res, string queries)
	{
		import std.algorithm : min, max;

		auto conn = client.lockConnection();
		scope(exit)	delete conn;

		int count = 1;
		try count = min(max(queries.to!int, 1), 500);
		catch (ConvException e) {}

		scope data = new WorldResponse[count];
		foreach (ref w; data) {
			immutable id = _uniformVariable(_gen);
			immutable sid = id.to!string;
			immutable query = "SELECT randomNumber FROM world WHERE id = " ~  sid;
			immutable result = conn.execStatement(query, ValueFormat.BINARY).rangify.front;
			w = WorldResponse(id, result[0].as!PGinteger);

			// update random number
			w.randomNumber = _uniformVariable(_gen);

			// persist to DB
			conn.execStatement("UPDATE world SET randomNumber = " ~ w.randomNumber.to!string ~ "  WHERE id = " ~ sid);

		}

		// write response as JSON
		res.writeJsonBody(data, HTTPStatus.ok, "application/json");
	}

	// GET /plaintext
	void getPlaintext(HTTPServerResponse res)
	{
		res.writeBody("Hello, World!", HTTPStatus.ok, "text/plain");
	}
}

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

static this()
{
	import std.process : environment;
	auto connectionInfo = "host=tfb-database port=5432 "
						~ "dbname=hello_world  user=benchmarkdbuser password=benchmarkdbpass";
	client = new PostgresClient(connectionInfo, poolSize);
}
