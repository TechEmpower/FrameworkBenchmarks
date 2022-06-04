import 'dart:async';
import 'dart:io';
import 'package:angel3_framework/angel3_framework.dart';
import 'package:angel3_orm/angel3_orm.dart';
import 'package:angel3_orm_postgres/angel3_orm_postgres.dart';
import 'package:postgres_pool/postgres_pool.dart';

Future<void> configureServer(Angel app) async {
  var logger = app.environment.isProduction ? null : app.logger;

  var connection = await connectToPostgres(app.configuration);
  await connection.open();
  var executor = PostgreSqlExecutor(connection, logger: logger);

  //var executor = await connectToPostgresPool(app.configuration, logger);

  app
    ..container!.registerSingleton<QueryExecutor>(executor)
    ..shutdownHooks.add((_) => connection.close());
//    ..shutdownHooks.add((_) => executor.close());
}

Future<PostgreSQLConnection> connectToPostgres(Map configuration) async {
  var postgresConfig = configuration['postgres'] as Map? ?? {};
  var connection = PostgreSQLConnection(
      postgresConfig['host'] as String? ?? 'localhost',
      postgresConfig['port'] as int? ?? 5432,
      postgresConfig['database_name'] as String? ??
          Platform.environment['USER'] ??
          Platform.environment['USERNAME'] ??
          '',
      username: postgresConfig['username'] as String?,
      password: postgresConfig['password'] as String?,
      timeZone: postgresConfig['time_zone'] as String? ?? 'UTC',
      timeoutInSeconds: postgresConfig['timeout_in_seconds'] as int? ?? 30,
      useSSL: postgresConfig['use_ssl'] as bool? ?? false);
  return connection;
}

Future<PostgreSqlPoolExecutor> connectToPostgresPool(
    Map configuration, dynamic logger) async {
  var postgresConfig = configuration['postgres'] as Map? ?? {};
  var _pool = PgPool(
    PgEndpoint(
        host: postgresConfig['host'] as String? ?? 'localhost',
        port: postgresConfig['port'] as int? ?? 5432,
        database: postgresConfig['database_name'] as String? ??
            Platform.environment['USER'] ??
            Platform.environment['USERNAME'] ??
            '',
        username: postgresConfig['username'] as String?,
        password: postgresConfig['password'] as String?),
    settings: PgPoolSettings()
      ..maxConnectionAge = Duration(hours: 1)
      ..concurrency = 10,
  );

  // Run sql to create the tables in a transaction
  //await _pool.runTx((conn) async {
  //  for (var s in schemas) {
  //    await conn.execute(await File('test/migrations/$s.sql').readAsString());
  //  }
  //});

  return PostgreSqlPoolExecutor(_pool, logger: logger);
}
