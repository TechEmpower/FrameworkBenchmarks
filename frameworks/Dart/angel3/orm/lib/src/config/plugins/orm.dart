import 'dart:async';
import 'dart:io';
import 'package:angel3_framework/angel3_framework.dart';
import 'package:angel3_orm/angel3_orm.dart';
import 'package:angel3_orm_postgres/angel3_orm_postgres.dart';
import 'package:postgres/postgres.dart';

Future<void> configureServer(Angel app) async {
  var logger = app.environment.isProduction ? null : app.logger;

  var connector = await pooledPostgresConnections(app.configuration);
  var executor = PostgreSqlPoolExecutor(connector, logger: logger);

  app
    ..container.registerSingleton<QueryExecutor>(executor)
    ..shutdownHooks.add((_) => connector.close());
}

Future<Pool<dynamic>> pooledPostgresConnections(Map configuration) async {
  var postgresConfig = configuration['postgres'] as Map? ?? {};
  return Pool.withEndpoints([
    Endpoint(
        host: postgresConfig['host'] as String? ?? 'localhost',
        port: postgresConfig['port'] as int? ?? 5432,
        database: postgresConfig['database_name'] as String? ??
            Platform.environment['USER'] ??
            Platform.environment['USERNAME'] ??
            '',
        username: postgresConfig['username'] as String?,
        password: postgresConfig['password'] as String?)
  ],
      settings: PoolSettings(
          maxConnectionAge: Duration(hours: 1),
          maxConnectionCount: 20,
          connectTimeout: Duration(
              seconds: postgresConfig['timeout_in_seconds'] as int? ?? 30),
          timeZone: postgresConfig['time_zone'] as String? ?? 'UTC',
          sslMode: SslMode.disable));
}
