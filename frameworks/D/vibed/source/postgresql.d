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
enum poolSize = 64;

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
	import std.datetime : seconds;
	auto router = new URLRouter;
	router.registerWebInterface(new WebInterface);
	router.rebuild();

	auto settings = new HTTPServerSettings;
	settings.keepAliveTimeout = 20.seconds;
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
		scope(exit)	destroy(conn);

		int id = _uniformVariable(_gen);
		QueryParams qp;
		qp.preparedStatementName("db_prpq");
		qp.resultFormat = ValueFormat.BINARY;
		qp.argsVariadic(id);
		immutable result = conn.execPrepared(qp).rangify.front;
		auto w = WorldResponse(id, result[0].as!PGinteger);
		res.writeJsonBody(w, HTTPStatus.ok, "application/json");
	}

	// GET /queries?queries=...
	void getQueries(HTTPServerResponse res, string queries)
	{
		import std.algorithm : min, max;
		import std.uni : isNumber;

		auto conn = client.lockConnection();
		scope(exit)	destroy(conn);

		// Convert the "queries" parameter to int and ignore any conversion errors
		// Note that you'd usually declare queries as int instead. However, the
		// test required to gracefully handle errors here.
		int count = 1;
		if (queries.length && isNumber(queries[0]))
		{
			try count = min(max(queries.to!int, 1), 500);
			catch (ConvException) {}
		}

		// assemble the response array
		scope data = new WorldResponse[count];
		QueryParams qp;
		qp.preparedStatementName("db_prpq");
		qp.resultFormat = ValueFormat.BINARY;
		foreach (ref w; data) {
			int id = _uniformVariable(_gen);
			qp.argsVariadic(id);
			immutable query = "SELECT randomNumber, id FROM world WHERE id = " ~  id.to!string;
			immutable result = conn.execPrepared(qp).rangify.front;
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
		scope(exit)	destroy(conn);

		FortuneResponse[] data;
		QueryParams qp;
		qp.preparedStatementName("fortune_prpq");
		auto result = conn.execPrepared(qp).rangify;
		data = result.map!(f => FortuneResponse(f[0].as!PGinteger, f[1].as!PGtext)).array;
		data ~= FortuneResponse(0, "Additional fortune added at request time.");
		data.sort!((a, b) => a.message < b.message);
		render!("fortunes.dt", data);
	}

	// GET /updates?queries=...
	void getUpdates(HTTPServerResponse res, string queries)
	{
		import std.algorithm : min, max;
		import std.uni : isNumber;

		auto conn = client.lockConnection();
		scope(exit)	destroy(conn);

		int count = 1;
		if (queries.length && isNumber(queries[0]))
		{
			try count = min(max(queries.to!int, 1), 500);
			catch (ConvException) {}
		}

		scope data = new WorldResponse[count];
		QueryParams qp;
		qp.preparedStatementName("db_prpq");
		qp.resultFormat = ValueFormat.BINARY;

		QueryParams qp_update;
		qp_update.preparedStatementName("db_update_prpq");
		qp_update.resultFormat = ValueFormat.BINARY;

		foreach (ref w; data) {
			int id = _uniformVariable(_gen);
			qp.argsVariadic(id);
			immutable query = "SELECT id, randomNumber FROM world WHERE id = " ~  id.to!string;
			immutable result = conn.execPrepared(qp).rangify.front;
			w = WorldResponse(id, result[0].as!PGinteger);

			// update random number
			w.randomNumber = _uniformVariable(_gen);
			qp_update.argsVariadic(w.randomNumber, id);
			// persist to DB
			conn.sendQueryPrepared(qp_update);
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
	client = new PostgresClient(connectionInfo, poolSize, (Connection cn){
		cn.prepare("fortune_prpq", "SELECT id, message::text FROM Fortune");
		cn.prepare("db_prpq", "SELECT randomNumber, id FROM world WHERE id = $1");
		cn.prepare("db_update_prpq", "UPDATE world SET randomNumber = $1  WHERE id = $2");
	} );
}
