import 'dart:async';
import '../models/models.dart';

abstract class Querier {
  Future<List<Fortune>> getFortunes();

  Future<World> getRandomWorld();

  Future<World> updateWorld(int id, World world);
}
