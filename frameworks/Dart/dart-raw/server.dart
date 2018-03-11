import 'dart:async' show Future;
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:math' show Random;
import 'package:args/args.dart' show ArgParser;
import 'package:mustache/mustache.dart' as mustache;
import 'package:postgresql/postgresql.dart' as pg;
import 'package:postgresql/postgresql_pool.dart' as pgpool;
import 'package:yaml/yaml.dart' as yaml;

final _encoder = new JsonUtf8Encoder();

/// Starts a new HTTP server that implements the tests to be benchmarked.  The
/// address and port for incoming connections is configurable via command line
/// arguments, as is the number of database connections to be maintained in the
/// connection pool.
void main(List<String> args) {
  var parser = new ArgParser()
    ..addOption('address', abbr: 'a', defaultsTo: '0.0.0.0')
    ..addOption('port', abbr: 'p', defaultsTo: '8080')
    ..addOption('dbconnections', abbr: 'd', defaultsTo: '256')
    ..addOption('isolates', abbr: 'i', defaultsTo: '1');
  var arguments = parser.parse(args);
  var isolates = int.parse(arguments['isolates']);
  var dbConnections = int.parse(arguments['dbconnections']) ~/ isolates;
  ServerSocket
      .bind(arguments['address'], int.parse(arguments['port']))
      .then((server) {
    var ref = server.reference;
    for (int i = 1; i < isolates; i++) {
      Isolate.spawn(startInIsolate, [ref, dbConnections]);
    }
    _startServer(server, dbConnections);
  });
}

void startInIsolate(List args) {
  var ref = args[0];
  var dbConnections = args[1];
  ref.create().then((server) {
    _startServer(server, dbConnections);
  });
}

/// The entity used in the database query and update tests.
class World {
  int id;

  int randomnumber;

  World(this.id, this.randomnumber);

  toJson() => {'id': id, 'randomNumber': randomnumber};
}

/// The entity used in the fortunes test.
class Fortune implements Comparable<Fortune> {
  int id;
  String message;

  Fortune(this.id, this.message);

  int compareTo(Fortune other) => message.compareTo(other.message);
}

/// The number of rows in the world entity table.
const _WORLD_TABLE_SIZE = 10000;

/// A random number generator.
final _RANDOM = new Random();

/// The PostgreSQL connection pool used by all the tests that require database
/// connectivity.
pgpool.Pool _connectionPool;

/// The mustache template which is rendered in the fortunes test.
mustache.Template _fortunesTemplate;

/// Starts a benchmark server, which listens for connections from
/// '[address] : [port]' and maintains [dbConnections] connections to the
/// database.
void _startServer(serverSocket, dbConnections) {
  Future.wait([
    new File('postgresql.yaml').readAsString().then((config) {
      _connectionPool = new pgpool.Pool(
          new pg.Settings.fromMap(yaml.loadYaml(config)).toUri(),
          min: dbConnections, max: dbConnections);
      return _connectionPool.start();
    }),
    new File('fortunes.mustache').readAsString().then((template) {
      _fortunesTemplate = mustache.parse(template);
    })
  ]).then((_) {
    var server = new HttpServer.listenOn(serverSocket);
    server.defaultResponseHeaders.clear();
    server.serverHeader = 'dart';
    server.listen((request) {
      switch (request.uri.path) {
        case '/json':
          _jsonTest(request);
          break;
        case '/db':
          _dbTest(request);
          break;
        case '/queries':
          _queriesTest(request);
          break;
        case '/fortunes':
          _fortunesTest(request);
          break;
        case '/updates':
          _updatesTest(request);
          break;
        case '/plaintext':
          _plaintextTest(request);
          break;
        default:
          _sendResponse(request, HttpStatus.NOT_FOUND);
          break;
      }
    });
  });
}

/// Returns the given [text] parsed as a base 10 integer.  If the text is null
/// or is an otherwise invalid representation of a base 10 integer, zero is
/// returned.
int _parseInt(String text) =>
    (text == null) ? 0 : int.parse(text, radix: 10, onError: ((_) => 0));

/// Completes the given [request] by writing the [response] with the given
/// [statusCode] and [type].
void _sendResponse(HttpRequest request, int statusCode,
    {ContentType type, List<int> response}) {
  request.response.statusCode = statusCode;
  request.response.headers.date = new DateTime.now();
  if (type != null) {
    request.response.headers.contentType = type;
  }
  if (response != null) {
    request.response.contentLength = response.length;
    request.response.add(response);
  } else {
    request.response.contentLength = 0;
  }
  request.response.close();
}

/// Completes the given [request] by writing the [response] as HTML.
void _sendHtml(HttpRequest request, String response) {
  _sendResponse(request, HttpStatus.OK,
      type: ContentType.HTML, response: UTF8.encode(response));
}

/// Completes the given [request] by writing the [response] as JSON.
void _sendJson(HttpRequest request, Object response) {
  _sendResponse(request, HttpStatus.OK,
      type: ContentType.JSON, response: _encoder.convert(response));
}

/// Completes the given [request] by writing the [response] as plain text.
void _sendText(HttpRequest request, String response) {
  _sendResponse(request, HttpStatus.OK,
      type: ContentType.TEXT, response: UTF8.encode(response));
}

/// Responds with the JSON test to the [request].
void _jsonTest(HttpRequest request) {
  _sendJson(request, {'message': 'Hello, World!'});
}

Future<World> _queryRandom() {
  return _connectionPool.connect().then((connection) {
    return connection.query(
        'SELECT id, randomnumber FROM world WHERE id = @id;', {
      'id': _RANDOM.nextInt(_WORLD_TABLE_SIZE) + 1
    })
        //
        // The benchmark's constraints tell us there is exactly one row.
        //
        .single.then((row) => new World(row[0], row[1])).whenComplete(() {
      connection.close();
    });
  });
}

/// Responds with the database query test to the [request].
void _dbTest(HttpRequest request) {
  _queryRandom().then((response) => _sendJson(request, response));
}

/// Responds with the database queries test to the [request].
void _queriesTest(HttpRequest request) {
  var queries = _parseInt(request.uri.queryParameters['queries']).clamp(1, 500);
  Future
      .wait(new List.generate(queries, (_) => _queryRandom(), growable: false))
      .then((response) => _sendJson(request, response));
}

/// Responds with the fortunes test to the [request].
void _fortunesTest(HttpRequest request) {
  _connectionPool.connect().then((connection) {
    return connection
        .query('SELECT id, message FROM fortune;')
        .map((row) => new Fortune(row[0], row[1]))
        .toList()
        .whenComplete(() {
      connection.close();
    });
  }).then((fortunes) {
    fortunes.add(new Fortune(0, 'Additional fortune added at request time.'));
    fortunes.sort();
    _sendHtml(request, _fortunesTemplate.renderString({
      'fortunes': fortunes
          .map((fortune) => {'id': fortune.id, 'message': fortune.message})
          .toList()
    }));
  });
}

/// Responds with the updates test to the [request].
void _updatesTest(HttpRequest request) {
  var queries = _parseInt(request.uri.queryParameters['queries']).clamp(1, 500);
  Future.wait(new List.generate(queries, (_) {
    return _queryRandom().then((world) {
      world.randomnumber = _RANDOM.nextInt(_WORLD_TABLE_SIZE) + 1;
      return _connectionPool.connect().then((connection) {
        return connection
            .execute(
                'UPDATE world SET randomnumber = @randomnumber WHERE id = @id;',
                {'randomnumber': world.randomnumber, 'id': world.id})
            .whenComplete(() {
          connection.close();
        });
      }).then((_) => world);
    });
  }, growable: false)).then((worlds) => _sendJson(request, worlds));
}

/// Responds with the plaintext test to the [request].
void _plaintextTest(HttpRequest request) {
  _sendText(request, 'Hello, World!');
}
