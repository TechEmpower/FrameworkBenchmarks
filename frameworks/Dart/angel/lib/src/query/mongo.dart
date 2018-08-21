import 'dart:async';
import 'package:dart_angel_benchmark/src/models/fortune.dart';
import 'package:mongo_dart/mongo_dart.dart';
import '../models/models.dart';
import 'querier.dart';

class MongoQuerier implements Querier {
  final Db db;
  DbCollection _fortunes, _worlds;

  MongoQuerier(this.db) {
    _fortunes = db.collection('fortunes');
    _fortunes = db.collection('worlds');
  }

  @override
  Future<List<Fortune>> getFortunes() {
    return _fortunes.find().map(FortuneSerializer.fromMap).toList();
  }
}