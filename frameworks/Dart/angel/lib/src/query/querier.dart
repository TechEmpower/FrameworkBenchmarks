import 'dart:async';
import '../models/models.dart';

abstract class Querier {
  Future<List<Fortune>> getFortunes();
}