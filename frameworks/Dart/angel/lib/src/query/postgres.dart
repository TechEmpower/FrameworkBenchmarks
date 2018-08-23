import 'dart:async';
import 'package:dart_angel_benchmark/src/models/fortune.dart';
import 'package:postgres/postgres.dart';
import '../models/models.dart';
import 'querier.dart';

class PostgresQuerier implements Querier {
  final PostgreSQLConnection connection;

  PostgresQuerier(this.connection);

  static Fortune parseFortune(List row) {
    return Fortune(id: row[0], message: row[1]);
  }

  static World parseWorld(List row) {
    return World(id: row[0], randomNumber: row[1]);
  }

  @override
  Future<List<Fortune>> getFortunes() {
    return connection.query('SELECT id, message FROM fortune').then((rows) {
      return rows.map((parseFortune)).toList();
    });
  }

  @override
  Future<World> getRandomWorld() async {
    var rows = await connection
        .query('SELECT id, randomNumber FROM world ORDER BY RANDOM() LIMIT 1');
    return parseWorld(rows[0]);
  }

  @override
  Future<World> updateWorld(id, World world) async {
    await connection.query(
        'UPDATE world SET randomNumber = @randomNumber WHERE id = @id',
        substitutionValues: {'id': id, 'randomNumber': world.randomNumber});
    return world;
  }
}
