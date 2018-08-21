import 'dart:async';
import 'package:dart_angel_benchmark/src/models/fortune.dart';
import 'package:mongo_dart/mongo_dart.dart';
import '../models/models.dart';
import 'querier.dart';

class MongoQuerier implements Querier {
  final Db db;
  DbCollection _fortunes, _worlds;

  static Map<String, dynamic> transform(Map<String, dynamic> data) {
    return data..['id'] = (data['_id'] as ObjectId).toHexString();
  }

  MongoQuerier(this.db) {
    _fortunes = db.collection('fortunes');
    _fortunes = db.collection('worlds');
  }

  @override
  Future<List<Fortune>> getFortunes() {
    return _fortunes
        .find()
        .map(transform)
        .map(FortuneSerializer.fromMap)
        .toList();
  }

  @override
  Future<World> getRandomWorld() {
    return _worlds
        .aggregate([
          {
            r'$sample': {'size': 1}
          }
        ])
        .then(transform)
        .then(WorldSerializer.fromMap);
  }

  @override
  Future<World> updateWorld(id, World world) {
    return _worlds
        .update(where.id(ObjectId.fromHexString(id as String)), world.toJson())
        .then(transform)
        .then(WorldSerializer.fromMap);
  }
}
