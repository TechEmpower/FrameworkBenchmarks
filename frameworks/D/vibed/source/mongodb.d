import vibe.db.mongo.mongo;
import vibe.http.router;
import vibe.http.server;
import vibe.web.web;

import std.conv : ConvException, to;
import std.random : uniform;


enum worldSize = 10000;


shared static this()
{
	auto router = new URLRouter;
	router.registerWebInterface(new WebInterface);
	router.rebuild();

	auto settings = new HTTPServerSettings;
	settings.bindAddresses = ["0.0.0.0"];
	settings.options |= HTTPServerOption.distribute;
	settings.port = 8080;
	listenHTTP(settings, router);
}

MongoCollection _worldCollection;
MongoCollection _fortuneCollection;

// sets up the MongoDB connection pools for each thread
static this()
{
	import std.process : environment;
	auto db = connectMongoDB(environment["DBHOST"]);
	_worldCollection = db.getCollection("hello_world.World");
	_fortuneCollection = db.getCollection("hello_world.Fortune");
}

class WebInterface {
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
		struct Q { int _id; }
		auto query = Q(uniform(1, worldSize + 1));
		auto w = WorldResponse(_worldCollection.findOne!World(query));
		res.writeJsonBody(w, HTTPStatus.ok, "application/json");
	}

	// GET /queries?queries=...
	void getQueries(HTTPServerResponse res, string queries)
	{
		import std.algorithm : min, max;

		// Convert the "queries" parameter to int and ignore any conversion errors
		// Note that you'd usually declare queries as int instead. However, the
		// test required to gracefully handle errors here.
		int count = 1;
		try count = min(max(queries.to!int, 1), 500);
		catch (ConvException) {}

		// assemble the response array    
		scope data = new WorldResponse[count];
		foreach (ref w; data) {
			static struct Q { int _id; }
			auto query = Q(uniform(1, worldSize + 1));
			w = WorldResponse(_worldCollection.findOne!World(query));
		}

		// write response as JSON
		res.writeJsonBody(data, HTTPStatus.ok, "application/json");
	}

	// GET /fortunes
	void getFortunes()
	{
		import std.algorithm : map, sort;

		FortuneResponse[] data;
		data = _fortuneCollection.find!Fortune().map!(f => FortuneResponse(f)).array;
		data ~= FortuneResponse(Fortune(0, "Additional fortune added at request time."));
		data.sort!((a, b) => a.message < b.message);
		render!("fortunes.dt", data);
	}

	// GET /updates?queries=...
	void getUpdates(HTTPServerResponse res, string queries)
	{
		import std.algorithm : min, max;

		int count = 1;
		try count = min(max(queries.to!int, 1), 500);
		catch (ConvException e) {}

		scope data = new WorldResponse[count];
		foreach (ref w; data) {
			static struct Q { int _id; }
			auto query = Q(uniform(1, worldSize + 1));
			w = WorldResponse(_worldCollection.findOne!World(query));

			// update random number
			w.randomNumber = uniform(1, worldSize+1);

			// persist to DB
			static struct US {
				double randomNumber;
			}
			static struct U {
				@name("$set") US set;
			}
			_worldCollection.update(query, U(US(w.randomNumber)));
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

struct World {
	double _id;
	double randomNumber;
}

struct Fortune {
	double _id;
	string message;
}

struct WorldResponse {
	this(World w) { this.id = cast(int)w._id; this.randomNumber = cast(int)w.randomNumber; }
	int id;
	int randomNumber;
}

struct FortuneResponse {
	this(Fortune f) { this.id = cast(int)f._id; this.message = f.message; }
	int id;
	string message;
}
