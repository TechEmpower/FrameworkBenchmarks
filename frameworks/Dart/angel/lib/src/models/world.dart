import 'package:angel_serialize/angel_serialize.dart';
part 'world.g.dart';

@Serializable(autoIdAndDateFields: false)
abstract class _World {
  int get id;

  int get randomNumber;
}