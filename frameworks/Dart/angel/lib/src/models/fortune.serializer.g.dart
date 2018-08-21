// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'fortune.dart';

// **************************************************************************
// SerializerGenerator
// **************************************************************************

abstract class FortuneSerializer {
  static Fortune fromMap(Map map) {
    return new Fortune(id: map['id'] as int, message: map['message'] as String);
  }

  static Map<String, dynamic> toMap(Fortune model) {
    if (model == null) {
      return null;
    }
    return {'id': model.id, 'message': model.message};
  }
}

abstract class FortuneFields {
  static const List<String> allFields = const <String>[id, message];

  static const String id = 'id';

  static const String message = 'message';
}
