import 'dart:async' show Future;
import 'dart:io';
import 'dart:json' as json;
import 'dart:math' show Random;
import 'dart:utf' as utf;
import 'package:args/args.dart' show ArgParser, ArgResults;
import 'package:mustache/mustache.dart' as mustache;
import 'package:postgresql/postgresql.dart' as pg;
import 'package:postgresql/postgresql_pool.dart' as pgpool;
import 'package:yaml/yaml.dart' as yaml;

/**
 * Starts a new HTTP server that implements the tests to be benchmarked.  The
 * address and port for incoming connections is configurable via command line
 * arguments, as is the number of database connections to be maintained in the
 * connection pool.
 */
main() {
  ArgParser parser = new ArgParser();
  parser.addOption('address', abbr: 'a', defaultsTo: '0.0.0.0');
  parser.addOption('port', abbr: 'p', defaultsTo: '8080');
  parser.addOption('dbconnections', abbr: 'd', defaultsTo: '256');
  ArgResults arguments = parser.parse(new Options().arguments);
  _startServer(
      arguments['address'],
      int.parse(arguments['port']),
      int.parse(arguments['dbconnections']));
}

/**
 * The entity used in the database query and update tests.
 */
class World {
  int id;
  int randomNumber;

  World(this.id, this.randomNumber);

  toJson() => { 'id': id, 'randomNumber': randomNumber };
}

/**
 * The entity used in the fortunes test.
 */
class Fortune implements Comparable<Fortune> {
  int id;
  String message;

  Fortune(this.id, this.message);

  int compareTo(Fortune other) => message.compareTo(other.message);
}

/**
 * The number of rows in the world entity table.
 */
const _WORLD_TABLE_SIZE = 10000;

/**
 * A random number generator.
 */
final _RANDOM = new Random();

/**
 * The 'text/html; charset=utf-8' content type.
 */
final _TYPE_HTML = new ContentType('text', 'html', charset: 'utf-8');

/**
 * The 'application/json; charset=utf-8' content type.
 */
final _TYPE_JSON = new ContentType('application', 'json', charset: 'utf-8');

/**
 * The PostgreSQL connection pool used by all the tests that require database
 * connectivity.
 */
pgpool.Pool _connectionPool;

/**
 * The mustache template which is rendered in the fortunes test.
 */
mustache.Template _fortunesTemplate;

/**
 * Starts a benchmark server, which listens for connections from
 * '[address] : [port]' and maintains [dbConnections] connections to the
 * database.
 */
void _startServer(String address, int port, int dbConnections) {
  Future.wait([
    new File('postgresql.yaml').readAsString().then((String config) {
      _connectionPool = new pgpool.Pool(
          new pg.Settings.fromMap(yaml.loadYaml(config)).toUri(),
          min: dbConnections,
          max: dbConnections);
      return _connectionPool.start();
    }),
    new File('fortunes.mustache').readAsString().then((String template) {
      _fortunesTemplate = mustache.parse(template);
    })
  ]).then((_) {
    HttpServer.bind(address, port).then((HttpServer server) {
      server.listen((HttpRequest request) {
        switch (request.uri.path) {
          case '/':         return _jsonTest(request);
          case '/db':       return _dbTest(request);
          case '/fortunes': return _fortunesTest(request);
          case '/update':   return _updateTest(request);
          default:          return _sendResponse(request, HttpStatus.NOT_FOUND);
        }
      });
    });
  });
}

/**
 * Returns the given [text] parsed as a base 10 integer.  If the text is null
 * or is an otherwise invalid representation of a base 10 integer, zero is
 * returned.
 */
int _parseInt(String text) =>
    (text == null) ? 0 : int.parse(text, radix: 10, onError: ((_) => 0));

/**
 * Completes the given [request] by writing the [response] with the given
 * [statusCode] and [type].
 */
void _sendResponse(HttpRequest request, int statusCode,
                   [ ContentType type, String response ]) {
  request.response.statusCode = statusCode;
  request.response.headers.add(
      HttpHeaders.CONNECTION,
      (request.persistentConnection) ? 'keep-alive' : 'close');
  request.response.headers.add(HttpHeaders.SERVER, 'dart');
  request.response.headers.date = new DateTime.now();
  if (type != null) {
    request.response.headers.contentType = type;
  }
  if (response != null) {
    //
    // A simpler way to write a response would be to:
    //
    //   1. Not set the contentLength header.
    //   2. Use response.write instead of response.add.
    //
    // However, doing that results in a chunked, gzip-encoded response, and
    // gzip is explicitly disallowed by the requirements for these benchmark
    // tests.
    //
    // See:  http://www.techempower.com/benchmarks/#section=code
    //
    List<int> encodedResponse = utf.encodeUtf8(response);
    request.response.headers.contentLength = encodedResponse.length;
    request.response.add(encodedResponse);
  }
  //
  // The load-testing tool will close any currently open connection at the end
  // of each run.  That potentially causes an error to be thrown here.  Since
  // we want the server to remain alive for subsequent runs, we catch the
  // error.
  //
  request.response.close().catchError(print);
}

/**
 * Completes the given [request] by writing the [response] as HTML.
 */
void _sendHtml(HttpRequest request, String response) {
  _sendResponse(request, HttpStatus.OK, _TYPE_HTML, response);
}

/**
 * Completes the given [request] by writing the [response] as JSON.
 */
void _sendJson(HttpRequest request, Object response) {
  _sendResponse(request, HttpStatus.OK, _TYPE_JSON, json.stringify(response));
}

/**
 * Responds with the JSON test to the [request].
 */
void _jsonTest(HttpRequest request) {
  _sendJson(request, { 'message': 'Hello, World!' });
}

/**
 * Responds with the database query test to the [request].
 */
void _dbTest(HttpRequest request) {
  int queries = _parseInt(request.queryParameters['queries']).clamp(1, 500);
  List<World> worlds = new List<World>(queries);
  Future.wait(new List.generate(queries, (int index) {
    return _connectionPool.connect().then((pg.Connection connection) {
      return connection.query(
              'SELECT id, randomNumber FROM world WHERE id = @id;',
              { 'id': _RANDOM.nextInt(_WORLD_TABLE_SIZE) + 1 })
          .map((row) => new World(row[0], row[1]))
          //
          // The benchmark's constraints tell us there is exactly one row here.
          //
          .forEach((World world) { worlds[index] = world; })
          .then((_) { connection.close(); });
    });
  }, growable: false)).then((_) { _sendJson(request, worlds); });
}

/**
 * Responds with the fortunes test to the [request].
 */
void _fortunesTest(HttpRequest request) {
  List<Fortune> fortunes = [];
  _connectionPool.connect().then((pg.Connection connection) {
    return connection.query('SELECT id, message FROM fortune;')
        .map((row) => new Fortune(row[0], row[1]))
        .forEach(fortunes.add)
        .then((_) { connection.close(); });
  }).then((_) {
    fortunes.add(new Fortune(0, 'Additional fortune added at request time.'));
    fortunes.sort();
    _sendHtml(request, _fortunesTemplate.renderString({
      'fortunes': fortunes.map((Fortune fortune) => {
              'id': fortune.id, 'message': fortune.message
          }).toList()
    }));
  });
}

/**
 * Responds with the updates test to the [request].
 */
void _updateTest(HttpRequest request) {
  int queries = _parseInt(request.queryParameters['queries']).clamp(1, 500);
  List<World> worlds = new List<World>(queries);
  Future.wait(new List.generate(queries, (int index) {
    return _connectionPool.connect().then((pg.Connection connection) {
      return connection.query(
              'SELECT id, randomNumber FROM world WHERE id = @id;',
              { 'id': _RANDOM.nextInt(_WORLD_TABLE_SIZE) + 1 })
          .map((row) => new World(row[0], row[1]))
          //
          // The benchmark's constraints tell us there is exactly one row here.
          //
          .forEach((World world) { worlds[index] = world; })
          .then((_) { connection.close(); });
    });
  }, growable: false)).then((_) {
    Future.wait(new List.generate(queries, (int index) {
      World world = worlds[index];
      world.randomNumber = _RANDOM.nextInt(_WORLD_TABLE_SIZE) + 1;
      return _connectionPool.connect().then((pg.Connection connection) {
        return connection.execute(
                'UPDATE world SET randomNumber = @randomNumber WHERE id = @id;',
                { 'randomNumber': world.randomNumber, 'id': world.id })
            .then((_) { connection.close(); });
      });
    }, growable: false)).then((_) { _sendJson(request, worlds); });
  });
}
