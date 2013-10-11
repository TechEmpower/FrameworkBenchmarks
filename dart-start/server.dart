import "dart:core";
import "dart:io";
import 'dart:async' show Future;
import 'dart:json' as json;
import 'dart:math' show Random;
import "package:start/start.dart";
import "package:args/args.dart";
import 'package:postgresql/postgresql.dart' as pg;
import 'package:postgresql/postgresql_pool.dart' as pgpool;
import 'package:yaml/yaml.dart' as yaml;
import 'package:mustache/mustache.dart' as mustache;
import 'package:mongo_dart/mongo_dart.dart';
import 'package:crypto/crypto.dart';

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
    }),
    new File('fortunes.mustache').readAsString().then((template) {
      _fortunesTemplate = mustache.parse(template);
    })
  ]).then((_) {
    start(host: arguments["address"], public: 'web', port: int.parse(arguments["port"]))
      .then((Server app) {
        
        // JSON test
        app.get('/json').listen((request) {
          
          var helloWorld = {
            "message": "Hello, World!"
          };
          
          _setJsonHeaders(request.response);
          request.response.send(json.stringify(helloWorld));
        });
        
        
        // Query test
        app.get("/db").listen((request) {
          
          _setJsonHeaders(request.response);
          
          _query().then((data) {
            request.response.send(json.stringify(data));
          });
        });
        
        // Queries test
        app.get("/queries").listen((request) {
          
          var queries = _parseQueriesParam(request.param("queries"));
          
          _setJsonHeaders(request.response);
          
          Future.wait(
                new List.generate(
                  queries,
                  (_) => _query(),
                  growable: false
                )
            )
            .then((response) => request.response.send(json.stringify(response)));
        });
        
        // Fortunes test
        app.get("/fortunes").listen((request) {
          _setHtmlHeaders(request.response);
          
          _connectionPool.connect().then((connection) {
            return connection.query('SELECT "id", "message" FROM "Fortune";')
                .map((row) => new Fortune(row[0], row[1]))
                .toList()
                .whenComplete(() { connection.close(); });
          }).then((fortunes) {
            fortunes.add(new Fortune(0, 'Additional fortune added at request time.'));
            fortunes.sort();
            request.response.send(_fortunesTemplate.renderString({
              "fortunes": fortunes.map((fortune) => {
                "id": fortune.id,
                "message": fortune.message
              }).toList()
            }));
          });
        });
        
        // Updates test
        app.get("/updates").listen((request) {
          
          var queries = _parseQueriesParam(request.param("queries"));
          
          _setJsonHeaders(request.response);
          
          Future.wait(new List.generate(queries, (_) {
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
          .then((worlds) => request.response.send(json.stringify(worlds)));
        });
        
        // Plain text test
        app.get("/plaintext").listen((request) {
          _setPlainHeaders(request.response);
          request.response.send("Hello, World!");
        });
        
        // Mongo World dev data generation
        app.get("/generate-world").listen((request) {
          
          _worldCollection.drop()
            .then((_) {
              var collectionData = new List.generate(_WORLD_TABLE_SIZE, (index) {
                return {
                  "id": index + 1,
                  "randomnumber": _RANDOM.nextInt(_WORLD_TABLE_SIZE)
                };
              });
              return _worldCollection.insertAll(collectionData); 
            })
            .then((_) {
              request.response.send("Generated");
            });
        });
        
        // Mongo Fortune dev data generation
        app.get("/generate-fortune").listen((request) {
          _fortuneCollection.drop()
            .then((_) {
              var collectionData = new List.generate(_FORTUNE_TABLE_SIZE, (index) {
                var hash = new MD5();
                hash.add(_RANDOM.nextInt(_FORTUNE_TABLE_SIZE).toString().codeUnits);
                return {
                  "id": index + 1,
                  "message": CryptoUtils.bytesToHex(hash.close())
                };
              });
              return _fortuneCollection.insertAll(collectionData); 
            })
            .then((_) {
              request.response.send("Generated");
            });
          
        });
        
        // Mongo query test
        app.get("/db-mongo").listen((request) {

          _setJsonHeaders(request.response);

          _mongoQuery().then((data) {
            request.response.json({
              "id": data["id"],
              "randomnumber": data["randomnumber"]
            });
          });
        });
        
        // Mongo queries test
        app.get("/queries-mongo").listen((request) {
          
          var queries = _parseQueriesParam(request.param("queries"));

          _setJsonHeaders(request.response);
          
          Future.wait(
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
              request.response.send(json.stringify(results.toList()));
            });
        });
        
        // Mongo updates test
        app.get("/updates-mongo").listen((request) {
          var queries = _parseQueriesParam(request.param("queries"));

          _setJsonHeaders(request.response);

          Future.wait(new List.generate(queries, (index) {
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
            request.response.send(json.stringify(result.toList()));
          });
        });
        
        
        // Mongo fortunes test
        app.get("/fortunes-mongo").listen((request) {
          
          _setHtmlHeaders(request.response);
          
          _fortuneCollection.find().toList().then((fortunes) {
            fortunes = fortunes.map((fortune) {
              return new Fortune(fortune["id"], fortune["message"]);
            }).toList();
            fortunes.add(new Fortune(0, 'Additional fortune added at request time.'));
            fortunes.sort();
            request.response.send(_fortunesTemplate.renderString({
              "fortunes": fortunes.map((fortune) => {
                "id": fortune.id,
                "message": fortune.message
              }).toList()
            }));
          });
        });
        
      });
  });
}

// set JSON headers
_setJsonHeaders(response) {
  _setHeaders(response);
  response
    .header(HttpHeaders.CONTENT_TYPE, 'application/json; charset=UTF-8');   
}

// set plain text headers
_setPlainHeaders(response) {
  _setHeaders(response);
  response
    .header(HttpHeaders.CONTENT_TYPE, 'text/plain; charset=UTF-8');
}

// set HTML headers
_setHtmlHeaders(response) {
  _setHeaders(response);
  response
    .header(HttpHeaders.CONTENT_TYPE, 'text/html; charset=UTF-8');
}

// set common headers
_setHeaders(response) {
  // disable gzip encoding
  response.header(HttpHeaders.CONTENT_ENCODING, "")
    ..header(HttpHeaders.DATE, new DateTime.now());
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
