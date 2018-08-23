import 'package:angel_serialize/angel_serialize.dart';
part 'fortune.g.dart';
part 'fortune.serializer.g.dart';

@Serializable(autoIdAndDateFields: false, autoSnakeCaseNames: false)
abstract class _Fortune {
  num get id;

  String get message;
}
