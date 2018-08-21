import 'dart:async';
import '../models/fortune.dart';

abstract class Querier {
  Stream getFortunes();
}