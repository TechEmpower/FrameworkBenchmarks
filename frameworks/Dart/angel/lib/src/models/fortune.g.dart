// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'fortune.dart';

// **************************************************************************
// JsonModelGenerator
// **************************************************************************

@generatedSerializable
class Fortune implements _Fortune {
  const Fortune({this.id, this.message});

  @override
  final int id;

  @override
  final String message;

  Fortune copyWith({int id, String message}) {
    return new Fortune(id: id ?? this.id, message: message ?? this.message);
  }

  bool operator ==(other) {
    return other is _Fortune && other.id == id && other.message == message;
  }

  Map<String, dynamic> toJson() {
    return FortuneSerializer.toMap(this);
  }
}
