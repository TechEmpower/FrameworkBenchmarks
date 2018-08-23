import 'package:angel_serialize/angel_serialize.dart';
part 'world.g.dart';
part 'world.serializer.g.dart';

@Serializable(autoIdAndDateFields: false, autoSnakeCaseNames: false)
abstract class _World {
  num get id;

  num get randomNumber;
}
