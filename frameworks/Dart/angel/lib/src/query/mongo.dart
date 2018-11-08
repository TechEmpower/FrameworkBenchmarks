import 'dart:async';
import 'dart:math';
import 'package:dart_angel_benchmark/src/models/fortune.dart';
import 'package:mongo_dart/mongo_dart.dart';
import '../models/models.dart';
import 'querier.dart';

class MongoQuerier implements Querier {
  final Db db;
  final Random rnd;
  final int worldTableSize;
  DbCollection _fortunes, _worlds;

  MongoQuerier(this.db, this.rnd, this.worldTableSize) {
    _fortunes = db.collection('fortune');
    _worlds = db.collection('world');
  }

  @override
  Future<List<Fortune>> getFortunes() {
    return _fortunes.find().map(FortuneSerializer.fromMap).toList();
  }

  @override
  Future<World> getRandomWorld() {
    return _worlds
        .findOne(where.skip(rnd.nextInt(worldTableSize)))
        .then(WorldSerializer.fromMap);
  }

  @override
  Future<World> updateWorld(id, World world) {
    return _worlds
        .update(where.eq('id', id), world.toJson())
        .then(WorldSerializer.fromMap);
  }
}
