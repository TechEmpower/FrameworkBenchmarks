library stream_benchmark;

import "dart:core";
import "dart:io";
import 'dart:async' show Future;
import 'dart:json' as json;
import 'dart:math' show Random;
import "package:stream/stream.dart";
import "package:args/args.dart";
import 'package:postgresql/postgresql.dart' as pg;
import 'package:postgresql/postgresql_pool.dart' as pgpool;
import 'package:yaml/yaml.dart' as yaml;
import 'package:mongo_dart/mongo_dart.dart';

part "fortunesView.rsp.dart";

// URI mapping to request handlers
var _uriMapping = {
  "/db": _dbTest,
  "/json": _jsonTest,
  "/queries": _queriesTest,
  "/updates": _updatesTest,
  "/fortunes": _fortunesTest,
  "/plaintext": _plaintextTest,
  "/db-mongo": _dbMongoTest,
  "/queries-mongo": _queriesMongoTest,
  "/updates-mongo": _updatesMongoTest,
  "/fortunes-mongo": _fortunesMongoTest
};

// filter map to set the headers for the request
var _filterMapping = {
  "/db": _jsonHeadersFilter,
  "/json": _jsonHeadersFilter,
  "/queries": _jsonHeadersFilter,
  "/updates": _jsonHeadersFilter,
  "/fortunes": _htmlHeadersFilter,
  "/plaintext": _plainHeadersFilter,
  "/db-mongo": _jsonHeadersFilter,
  "/queries-mongo": _jsonHeadersFilter,
  "/updates-mongo": _jsonHeadersFilter,
  "/fortunes-mongo": _htmlHeadersFilter
};

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

  int randomnumber;

  World(this.id, this.randomnumber);

  toJson() => { "id": id, "randomnumber": randomnumber };
}

main() {
  var parser = new ArgParser();
  parser.addOption('address', abbr: 'a', defaultsTo: '0.0.0.0');
  parser.addOption('port', abbr: 'p', defaultsTo: '8080');
  parser.addOption('dbconnections', abbr: 'd', defaultsTo: '256');
  
  var arguments = parser.parse(new Options().arguments);
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
     new StreamServer(uriMapping: _uriMapping, filterMapping: _filterMapping).start(address: arguments["address"], port: int.parse(arguments["port"]));
   });
}

_jsonTest(HttpConnect connect) {
  var helloWorld = {
      "message": "Hello, World!"
  };
  
  connect.response.write(json.stringify(helloWorld));
}

_dbTest(HttpConnect connect) {
  
  return _query().then((data) {
    connect.response.write(json.stringify(data));
  });
}

_queriesTest(HttpConnect connect) {
  var queries = _parseQueriesParam(connect.request.uri.queryParameters["queries"]);
  
  return Future.wait(
        new List.generate(
            queries,
            (_) => _query(),
            growable: false
        )
    )
    .then((response) => connect.response.write(json.stringify(response)));
}

_updatesTest(HttpConnect connect) {
  var queries = _parseQueriesParam(connect.request.uri.queryParameters["queries"]);
  
  return Future.wait(new List.generate(queries, (_) {
      return _query()
          .then((world) {
            world.randomnumber = _RANDOM.nextInt(_WORLD_TABLE_SIZE) + 1;
            return _connectionPool.connect()
              .then((connection) {
                return connection.execute(
                      'UPDATE "World" SET "randomnumber" = @randomnumber WHERE "id" = @id;',
                      { 
                        'randomnumber': world.randomnumber,
                        'id': world.id 
                      }
                  )
                  .whenComplete(() { connection.close(); });
                })
                .then((_) => world);
          });
    }, growable: false))
    .then((worlds) => connect.response.write(json.stringify(worlds)));
}

_fortunesTest(HttpConnect connect) {
  
  return _connectionPool.connect().then((connection) {
    return connection.query('SELECT "id", "message" FROM "Fortune";')
        .map((row) => new Fortune(row[0], row[1]))
          .toList()
            .whenComplete(() { connection.close(); });
  }).then((fortunes) {
    fortunes.add(new Fortune(0, 'Additional fortune added at request time.'));
    fortunes.sort();
    return fortunesView(connect, fortunes: fortunes);
  });
}

_plaintextTest(HttpConnect connect) {
 
  connect.response.write("Hello, World!");
}

_dbMongoTest(HttpConnect connect) {
  
  return _mongoQuery().then((data) {
    connect.response.write(json.stringify({
      "id": data["id"],
      "randomnumber": data["randomnumber"]
    }));
  });
}

_queriesMongoTest(HttpConnect connect) {
  var queries = _parseQueriesParam(connect.request.uri.queryParameters["queries"]);
  
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
          "randomnumber": world["randomnumber"]
        };
      });
      connect.response.write(json.stringify(results.toList()));
    });
}

_updatesMongoTest(HttpConnect connect) {
  var queries = _parseQueriesParam(connect.request.uri.queryParameters["queries"]);
  
  return Future.wait(new List.generate(queries, (index) {
      return _mongoQuery()
          .then((world) {
            world["randomnumber"] = _RANDOM.nextInt(_WORLD_TABLE_SIZE);
            return _worldCollection.update( { "_id": world["_id"] }, world)
                .then((_) => world);
          });
    }, growable: false))
    .then((worlds) {
      var result = worlds.map((world) {
        return {
          "id": world["id"],
          "randomnumber": world["randomnumber"]
        };
      });
      connect.response.write(json.stringify(result.toList()));
    });
}

_fortunesMongoTest(HttpConnect connect) {
  
  return _fortuneCollection.find().toList().then((fortunes) {
    fortunes = fortunes.map((fortune) {
      return new Fortune(fortune["id"], fortune["message"]);
    }).toList();
    fortunes.add(new Fortune(0, 'Additional fortune added at request time.'));
    fortunes.sort();
    return fortunesView(connect, fortunes: fortunes);
  });
}

// set common headers
_setHeaders(HttpResponse response) {
  // disable gzip encoding
  response.headers.set(HttpHeaders.CONTENT_ENCODING, "");
  response.headers.set(HttpHeaders.DATE, new DateTime.now());
}

// set JSON headers for matching requests
_jsonHeadersFilter(HttpConnect connect, Future chain(HttpConnect conn)) {
  _setHeaders(connect.response);
  connect.response.headers.set(HttpHeaders.CONTENT_TYPE, "application/json; charset=UTF-8");
  
  return chain(connect);
}

// set plain text headers for matching requests
_plainHeadersFilter(HttpConnect connect, Future chain(HttpConnect conn)) {
  _setHeaders(connect.response);
  connect.response.headers.set(HttpHeaders.CONTENT_TYPE, "text/plain; charset=UTF-8");
  
  return chain(connect);
}

// set HTML headers for matching requests
_htmlHeadersFilter(HttpConnect connect, Future chain(HttpConnect conn)) {
  _setHeaders(connect.response);
  connect.response.headers.set(HttpHeaders.CONTENT_TYPE, "text/html; charset=UTF-8");
  
  return chain(connect);
}

// parse queries param
_parseQueriesParam(param) {
  return param.isEmpty ? 1 : int.parse(param, radix: 10, onError: (_) => 1).clamp(1, 500);
}

// runs a query and returns a promise
_query() {
  return _connectionPool.connect().then((connection) {
    return connection
      .query('SELECT "id", "randomnumber" FROM "World" WHERE id = @id;', { 'id': _RANDOM.nextInt(_WORLD_TABLE_SIZE) + 1 })
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
