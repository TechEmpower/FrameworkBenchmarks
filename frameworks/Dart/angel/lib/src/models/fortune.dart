import 'package:angel_serialize/angel_serialize.dart';
part 'fortune.g.dart';
part 'fortune.serializer.g.dart';

@Serializable(autoIdAndDateFields: false)
abstract class _Fortune {
  int get id;

  String get message;
}