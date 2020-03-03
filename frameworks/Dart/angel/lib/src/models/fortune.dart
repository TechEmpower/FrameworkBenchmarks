import 'package:angel_serialize/angel_serialize.dart';
part 'fortune.g.dart';

@Serializable(autoSnakeCaseNames: false)
abstract class _Fortune {
  num get id;

  String get message;
}
