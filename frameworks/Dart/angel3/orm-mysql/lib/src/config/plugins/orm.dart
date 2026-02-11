import 'dart:async';
import 'dart:io';
import 'package:angel3_framework/angel3_framework.dart';
import 'package:angel3_orm/angel3_orm.dart';
import 'package:angel3_orm_mysql/angel3_orm_mysql.dart';
import 'package:mysql_client/mysql_client.dart';

// For Mysql
Future<void> configureServer(Angel app) async {
  try {
    var connection = await connectToMysql(app.configuration);
    var executor = MySqlExecutor(connection, logger: app.logger);

    app
      ..container.registerSingleton<QueryExecutor>(executor)
      ..shutdownHooks.add((_) => connection.close());
  } catch (e) {
    app.logger.severe("Failed to connect to MySQL. ORM disabled.", e);
  }
}

// Mysql Connection
Future<MySQLConnection> connectToMysql(Map configuration) async {
  var mysqlConfig = configuration['mysql'] as Map? ?? {};

  var connection = await MySQLConnection.createConnection(
      host: mysqlConfig['host'] as String? ?? 'localhost',
      port: mysqlConfig['port'] as int? ?? 3306,
      databaseName: mysqlConfig['database_name'] as String? ??
          Platform.environment['USER'] ??
          Platform.environment['USERNAME'] ??
          '',
      userName: mysqlConfig['username'] as String? ?? '',
      password: mysqlConfig['password'] as String? ?? '',
      secure: mysqlConfig['use_ssl'] as bool? ?? false);

  await connection.connect(timeoutMs: 30000);
  return connection;
}
