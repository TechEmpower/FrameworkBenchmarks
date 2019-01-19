import 'dart:async';
import '../models/models.dart';

abstract class Querier {
  Future<List<Fortune>> getFortunes();

  Future<World> getRandomWorld();

  Future<World> updateWorld(id, World world);
}
