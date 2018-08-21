// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'world.dart';

// **************************************************************************
// SerializerGenerator
// **************************************************************************

abstract class WorldSerializer {
  static World fromMap(Map map) {
    return new World(
        id: map['id'] as int, randomNumber: map['random_number'] as int);
  }

  static Map<String, dynamic> toMap(World model) {
    if (model == null) {
      return null;
    }
    return {'id': model.id, 'random_number': model.randomNumber};
  }
}

abstract class WorldFields {
  static const List<String> allFields = const <String>[id, randomNumber];

  static const String id = 'id';

  static const String randomNumber = 'random_number';
}
