import 'package:angel_serialize/angel_serialize.dart';
part 'world.g.dart';

@Serializable(autoSnakeCaseNames: false)
abstract class _World {
  num get id;

  num get randomNumber;
}
