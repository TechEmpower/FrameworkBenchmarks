import "dart:core";
import "dart:io";
import 'dart:async' show Future;
import 'dart:json' as json;
import 'dart:math' show Random;
import "package:express/express.dart";
import "package:args/args.dart";
import 'package:postgresql/postgresql.dart' as pg;
import 'package:postgresql/postgresql_pool.dart' as pgpool;
import 'package:yaml/yaml.dart' as yaml;
import 'package:mongo_dart/mongo_dart.dart';
import "views/jade.views.dart" as views;
import "public/jade.views.dart" as public;


// Postgres connection pool
var _connectionPool;

// Fortunes mustache template
var _fortunesTemplate;

// MongoDB connection
var _mongoDb;

// World Collection
var _worldCollection;
// Fortunes Collection
var _fortuneCollection;

// World table size
const _WORLD_TABLE_SIZE = 10000;
// Fortune table size used only for generation of data
const _FORTUNE_TABLE_SIZE = 100;

final _RANDOM = new Random();

class Fortune implements Comparable<Fortune> {
  int id;
  String message;

  Fortune(this.id, this.message);

  compareTo(Fortune other) => message.compareTo(other.message);
}

class World {
  int id;
  int randomNumber;

  World(this.id, this.randomNumber);

  toJson() => { "id": id, "randomNumber": randomNumber };
}

main() {
  var app = new Express();
  var parser = new ArgParser();
  parser.addOption('address', abbr: 'a', defaultsTo: '0.0.0.0');
  parser.addOption('port', abbr: 'p', defaultsTo: '8080');
  parser.addOption('dbconnections', abbr: 'd', defaultsTo: '256');
  
  var arguments = parser.parse(new Options().arguments);
  
  app.get("/json", _jsonTest)
    ..get("/db", _dbTest)
    ..get("/queries", _queriesTest)
    ..get("/updates", _updatesTest)
    ..get("/fortunes", _fortunesTest)
    ..get("/plaintext", _plaintextTest)
    ..get("/db-mongo", _dbMongoTest)
    ..get("/queries-mongo", _queriesMongoTest)
    ..get("/updates-mongo", _updatesMongoTest)
    ..get("/fortunes-mongo", _fortunesMongoTest)
    ..use(new JadeViewEngine(views.JADE_TEMPLATES, pages:public.JADE_TEMPLATES));
  
  Future.wait([
    new File("postgresql.yaml").readAsString().then((config){
      _connectionPool = new pgpool.Pool(
            new pg.Settings.fromMap(yaml.loadYaml(config)).toUri(),
            min: int.parse(arguments["dbconnections"]),
            max: int.parse(arguments["dbconnections"])
          );
      return _connectionPool.start();
    }),
    new File("mongodb.yaml").readAsString().then((config) {
      var mongoConfig = yaml.loadYaml(config);
      _mongoDb = new Db("mongodb://${mongoConfig["host"]}/${mongoConfig["database"]}");
      return _mongoDb.open().then((_) {
        _worldCollection = _mongoDb.collection("World");
        _fortuneCollection = _mongoDb.collection("Fortune");
      });
    })
  ]).then((_) {
    app.listen(arguments["address"], int.parse(arguments["port"]));
  });
}

// JSON test
_jsonTest(HttpContext ctx) {
  _setJsonHeaders(ctx.response);
  ctx.sendJson({
    "message": "Hello, World!"
  });
}

// Db test
_dbTest(HttpContext ctx) {
  _setJsonHeaders(ctx.response);
  return _query().then((data) {
    ctx.sendJson(data);
  });
}

// Queries test 
_queriesTest(HttpContext ctx) {
  var queries = _parseQueriesParam(ctx.req.uri.queryParameters["queries"]);
  
  _setJsonHeaders(ctx.res);
  
  return Future.wait(
        new List.generate(
            queries,
            (_) => _query(),
            growable: false
        )
    )
    .then((data) => ctx.sendJson(data));
}

// Updates test
_updatesTest(HttpContext ctx) {
  var queries = _parseQueriesParam(ctx.req.uri.queryParameters["queries"]);
  
  _setJsonHeaders(ctx.res);
  
  return Future.wait(new List.generate(queries, (_) {
      return _query()
          .then((world) {
            world.randomNumber = _RANDOM.nextInt(_WORLD_TABLE_SIZE) + 1;
            return _connectionPool.connect()
              .then((connection) {
                return connection.execute(
                      'UPDATE "World" SET "randomNumber" = @randomNumber WHERE "id" = @id;',
                      { 
                        'randomNumber': world.randomNumber,
                        'id': world.id 
                      }
                  )
                  .whenComplete(() { connection.close(); });
                })
                .then((_) => world);
          });
    }, growable: false))
    .then((worlds) => ctx.sendJson(worlds));
}

// Fortunes test
_fortunesTest(HttpContext ctx) {
  
  _setHtmlHeaders(ctx.res);
  
  return _connectionPool.connect().then((connection) {
    return connection.query('SELECT "id", "message" FROM "Fortune";')
        .map((row) => new Fortune(row[0], row[1]))
          .toList()
            .whenComplete(() { connection.close(); });
  }).then((fortunes) {
    fortunes.add(new Fortune(0, 'Additional fortune added at request time.'));
    fortunes.sort();
    ctx.render("fortunes-table", {
      "fortunes": fortunes
    });
  });
}

// Plain text test
_plaintextTest(HttpContext ctx) {
  
  _setPlainHeaders(ctx.res);
  
  ctx.sendText("Hello, World!");
}

// Mongo db test
_dbMongoTest(HttpContext ctx) {
  
  _setJsonHeaders(ctx.res);
  
  return _mongoQuery().then((data) {
    ctx.sendJson({
      "id": data["id"],
      "randomNumber": data["randomNumber"]
    });
  });
}

// Mongo queries test
_queriesMongoTest(HttpContext ctx) {
  var queries = _parseQueriesParam(ctx.req.uri.queryParameters["queries"]);
  
  _setJsonHeaders(ctx.res);
  
  return Future.wait(
        new List.generate(
            queries,
            (_) => _mongoQuery(),
            growable: false
        )
    )
    .then((response) {
      var results = response.map((world) {
        return {
          "id": world["id"],
          "randomNumber": world["randomNumber"]
        };
      });
      ctx.sendJson(results.toList());
    });
}

// Mongo updates test
_updatesMongoTest(HttpContext ctx) {
  var queries = _parseQueriesParam(ctx.req.uri.queryParameters["queries"]);
  
  _setJsonHeaders(ctx.res);
  
  return Future.wait(new List.generate(queries, (index) {
      return _mongoQuery()
          .then((world) {
            world["randomNumber"] = _RANDOM.nextInt(_WORLD_TABLE_SIZE);
            return _worldCollection.update( { "_id": world["_id"] }, world)
                .then((_) => world);
          });
    }, growable: false))
    .then((worlds) {
      var result = worlds.map((world) {
        return {
          "id": world["id"],
          "randomNumber": world["randomNumber"]
        };
      });
      ctx.sendJson(result.toList());
    });
}

// Fortunes Mongo test
_fortunesMongoTest(HttpContext ctx) {
  
  _setHtmlHeaders(ctx.res);
  
  return _fortuneCollection.find().toList().then((fortunes) {
    fortunes = fortunes.map((fortune) {
      return new Fortune(fortune["id"], fortune["message"]);
    }).toList();
    fortunes.add(new Fortune(0, 'Additional fortune added at request time.'));
    fortunes.sort();
    ctx.render("fortunes-table", {
      "fortunes": fortunes
    });
  });
}

// set common headers
_setHeaders(HttpResponse response) {
  // disable gzip encoding
  response.headers.set(HttpHeaders.CONTENT_ENCODING, "");
  response.headers.set(HttpHeaders.DATE, new DateTime.now());
}

// set JSON headers for matching requests
_setJsonHeaders(HttpResponse response) {
  _setHeaders(response);
  response.headers.set(HttpHeaders.CONTENT_TYPE, "application/json; charset=UTF-8");
}

// set plain text headers for matching requests
_setPlainHeaders(HttpResponse response) {
  _setHeaders(response);
  response.headers.set(HttpHeaders.CONTENT_TYPE, "text/plain; charset=UTF-8");
}

// set HTML headers for matching requests
_setHtmlHeaders(HttpResponse response) {
  _setHeaders(response);
  response.headers.set(HttpHeaders.CONTENT_TYPE, "text/html; charset=UTF-8");
}

// parse queries param
_parseQueriesParam(param) {
  return param.isEmpty ? 1 : int.parse(param, radix: 10, onError: (_) => 1).clamp(1, 500);
}

// runs a query and returns a promise
_query() {
  return _connectionPool.connect().then((connection) {
    return connection
      .query('SELECT "id", "randomNumber" FROM "World" WHERE id = @id;', { 'id': _RANDOM.nextInt(_WORLD_TABLE_SIZE) + 1 })
      .single
      .then((row) =>new World(row[0], row[1]))
      .whenComplete(() {
        connection.close();
      });
  });
}

// runs a mongo query and returns a promise
_mongoQuery() {
  return _worldCollection.findOne({
    "id": _RANDOM.nextInt(_WORLD_TABLE_SIZE) + 1
  });
}