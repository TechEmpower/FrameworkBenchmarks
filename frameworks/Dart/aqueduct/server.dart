import 'dart:async';
import 'dart:io';
import 'dart:math';

import 'package:aqueduct/aqueduct.dart';

export 'dart:async';
export 'dart:io';
export 'package:aqueduct/aqueduct.dart';

final _stripped_text = new ContentType("text", "plain");
final _stripped_json = new ContentType("application", "json");

final _random = new Random();

// World table size
const int _world_table_size = 10000;
// Fortune table size used only for generation of data
const int _FORTUNE_TABLE_SIZE = 100;

Future main() async {
  try {
    var app = new Application<DartAqueductBenchmarkSink>();
    var config = new ApplicationConfiguration()
      ..port = 8080
      ..configurationFilePath = "config.yaml";

    app.configuration = config;
    await app.start(numberOfInstances: 3, consoleLogging: false);
  } catch (e, st) {
    await writeError("$e\n $st");
  }
}

Future writeError(String error) async {
  print("$error");
}

class Fortune extends ManagedObject<_Fortune> implements _Fortune {}

class _Fortune {
  static String tableName() => "fortune";

  @managedPrimaryKey
  int id;

  String message;
}

class World extends ManagedObject<_World> implements _World {}

class _World {
  static String tableName() => "world";

  @managedPrimaryKey
  int id;

  int randomNumber;
}

/// This type initializes an application.
///
/// Override methods in this class to set up routes and initialize resources like
/// database connections. See http://aqueduct.io/docs/http/request_sink.
class DartAqueductBenchmarkSink extends RequestSink {
  /// Resource initialization code goes here.
  ///
  /// Resources like [AuthServer] and [PostgreSQLPersistentStore] should be instantiated
  /// in this constructor. Configuration behavior - like [HTTPCodecRepository.add] - should be
  /// configured in this constructor.
  ///
  /// The [appConfig] contains configuration data from `aqueduct serve`, e.g.
  /// the port the application is running on and the path to a configuration file.
  DartAqueductBenchmarkSink(ApplicationConfiguration appConfig)
      : super(appConfig) {
//    logger.onRecord.listen(
//        (rec) => print("$rec ${rec.error ?? ""} ${rec.stackTrace ?? ""}"));

    var options =
        new DartAqueductBenchmarkConfiguration(appConfig.configurationFilePath);
    ManagedContext.defaultContext = contextWithConnectionInfo(options.database);
  }

  /// All routes must be configured in this method.
  ///
  /// This method is invoked after the constructor and before [willOpen].
  /// All routes must be set up in this method and cannot be added after this method completes.
  @override
  void setupRouter(Router router) {
    router
        .route("/json")
        .listen((req) async => new Response.ok({"message": "Hello, World!"})
          ..contentType = _stripped_json
          ..headers["date"] = new DateTime.now());

    router
        .route("/plaintext")
        .listen((req) async => new Response.ok("Hello, World!")
          ..contentType = _stripped_text
          ..headers["date"] = new DateTime.now());

    router.route("/db").listen((req) async {
      ManagedObject<World> result = await getRandomWorldObject();
      return new Response.ok(result)
        ..contentType = _stripped_json
        ..headers["date"] = new DateTime.now();
    });

    router.route("/queries/[:queryCount]").listen((req) async {
      int queryCount = 1;
      if (req.path.variables.containsKey("queryCount")) {
        queryCount = int
            .parse(req.path.variables["queryCount"], onError: (_) => queryCount)
            .clamp(1, 500);
      }
      List<Future> resultFutures = [];
      for (int queryNumber = 0; queryNumber < queryCount; queryNumber++) {
        resultFutures.add(getRandomWorldObject());
      }
      List results = await Future.wait(resultFutures);
      return new Response.ok(results)
        ..contentType = _stripped_json
        ..headers["date"] = new DateTime.now();
    });

    router.route("/updates");

    router.route("/fortunes");
  }

  Future<ManagedObject<World>> getRandomWorldObject() async {
    int worldId = _random.nextInt(_world_table_size) + 1;
    Query query = new Query<World>()
      ..values.id = worldId;
    Future<ManagedObject<World>> result = query.fetchOne();
    return result;
  }

  /// Final initialization method for this instance.
  ///
  /// This method allows any resources that require asynchronous initialization to complete their
  /// initialization process. This method is invoked after [setupRouter] and prior to this
  /// instance receiving any requests.
  @override
  Future willOpen() async {}

  /*
   * Helper methods
   */

  ManagedContext contextWithConnectionInfo(
      DatabaseConnectionConfiguration connectionInfo) {
    var dataModel = new ManagedDataModel.fromCurrentMirrorSystem();
    var psc = new PostgreSQLPersistentStore.fromConnectionInfo(
        connectionInfo.username,
        connectionInfo.password,
        connectionInfo.host,
        connectionInfo.port,
        connectionInfo.databaseName);

    return new ManagedContext(dataModel, psc);
  }
}

/// An instance of this class reads values from a configuration
/// file specific to this application.
///
/// Configuration files must have key-value for the properties in this class.
/// For more documentation on configuration files, see https://aqueduct.io/docs/configure/ and
/// https://pub.dartlang.org/packages/safe_config.
class DartAqueductBenchmarkConfiguration extends ConfigurationItem {
  DartAqueductBenchmarkConfiguration(String fileName)
      : super.fromFile(fileName);

  DatabaseConnectionConfiguration database;
}
