import 'dart:async';
import 'dart:io';

import 'package:aqueduct/aqueduct.dart';

export 'dart:async';
export 'dart:io';
export 'package:aqueduct/aqueduct.dart';

Future main() async {
  try {
    var app = new Application<DartAqueductBenchmarkSink>();
    var config = new ApplicationConfiguration()
      ..port = 8081
      ..configurationFilePath = "config.yaml";

    app.configuration = config;

    await app.start(numberOfInstances: 3);
  } catch (e, st) {
    await writeError("$e\n $st");
  }
}

Future writeError(String error) async {
  print("$error");
}

class Fortune extends ManagedObject<_Fortune> implements _Fortune {}

class _Fortune {
  @managedPrimaryKey
  int id;

  String message;
}

class World extends ManagedObject<_World> implements _World {}

class _World {
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
        .listen((req) async => new Response.ok({"message": "Hello, World!"}));

    router
        .route("/plaintext")
        .listen((req) async => new Response.ok("Hello, World!")..contentType = ContentType.TEXT);
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
