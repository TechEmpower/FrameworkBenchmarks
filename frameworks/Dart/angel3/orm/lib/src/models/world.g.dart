// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'world.dart';

// **************************************************************************
// MigrationGenerator
// **************************************************************************

class WorldMigration extends Migration {
  @override
  void up(Schema schema) {
    schema.create('world', (table) {
      table.integer('id');
      table.integer('randomNumber');
    });
  }

  @override
  void down(Schema schema) {
    schema.drop('world');
  }
}

// **************************************************************************
// OrmGenerator
// **************************************************************************

class WorldQuery extends Query<World, WorldQueryWhere> {
  WorldQuery({Query? parent, Set<String>? trampoline}) : super(parent: parent) {
    trampoline ??= <String>{};
    trampoline.add(tableName);
    _where = WorldQueryWhere(this);
  }

  @override
  final WorldQueryValues values = WorldQueryValues();

  WorldQueryWhere? _where;

  @override
  Map<String, String> get casts {
    return {};
  }

  @override
  String get tableName {
    return 'world';
  }

  @override
  List<String> get fields {
    return const ['id', 'randomNumber'];
  }

  @override
  WorldQueryWhere? get where {
    return _where;
  }

  @override
  WorldQueryWhere newWhereClause() {
    return WorldQueryWhere(this);
  }

  static World? parseRow(List row) {
    if (row.every((x) => x == null)) {
      return null;
    }
    var model = World(id: (row[0] as int?), randomNumber: (row[1] as int?));
    return model;
  }

  @override
  Optional<World> deserialize(List row) {
    return Optional.ofNullable(parseRow(row));
  }
}

class WorldQueryWhere extends QueryWhere {
  WorldQueryWhere(WorldQuery query)
      : id = NumericSqlExpressionBuilder<int>(query, 'id'),
        randomNumber = NumericSqlExpressionBuilder<int>(query, 'randomNumber');

  final NumericSqlExpressionBuilder<int> id;

  final NumericSqlExpressionBuilder<int> randomNumber;

  @override
  List<SqlExpressionBuilder> get expressionBuilders {
    return [id, randomNumber];
  }
}

class WorldQueryValues extends MapQueryValues {
  @override
  Map<String, String> get casts {
    return {};
  }

  int? get id {
    return (values['id'] as int?);
  }

  set id(int? value) => values['id'] = value;
  int? get randomNumber {
    return (values['randomNumber'] as int?);
  }

  set randomNumber(int? value) => values['randomNumber'] = value;
  void copyFrom(World model) {
    id = model.id;
    randomNumber = model.randomNumber;
  }
}

// **************************************************************************
// JsonModelGenerator
// **************************************************************************

@generatedSerializable
class World extends _World {
  World({this.id, this.randomNumber});

  @override
  int? id;

  @override
  int? randomNumber;

  World copyWith({int? id, int? randomNumber}) {
    return World(
        id: id ?? this.id, randomNumber: randomNumber ?? this.randomNumber);
  }

  @override
  bool operator ==(other) {
    return other is _World &&
        other.id == id &&
        other.randomNumber == randomNumber;
  }

  @override
  int get hashCode {
    return hashObjects([id, randomNumber]);
  }

  @override
  String toString() {
    return 'World(id=$id, randomNumber=$randomNumber)';
  }

  Map<String, dynamic> toJson() {
    return WorldSerializer.toMap(this);
  }
}

// **************************************************************************
// SerializerGenerator
// **************************************************************************

const WorldSerializer worldSerializer = WorldSerializer();

class WorldEncoder extends Converter<World, Map> {
  const WorldEncoder();

  @override
  Map convert(World model) => WorldSerializer.toMap(model);
}

class WorldDecoder extends Converter<Map, World> {
  const WorldDecoder();

  @override
  World convert(Map map) => WorldSerializer.fromMap(map);
}

class WorldSerializer extends Codec<World, Map> {
  const WorldSerializer();

  @override
  WorldEncoder get encoder => const WorldEncoder();
  @override
  WorldDecoder get decoder => const WorldDecoder();
  static World fromMap(Map map) {
    return World(
        id: map['id'] as int?, randomNumber: map['randomNumber'] as int?);
  }

  static Map<String, dynamic> toMap(_World? model) {
    if (model == null) {
      return {};
    }
    return {'id': model.id, 'randomNumber': model.randomNumber};
  }
}

abstract class WorldFields {
  static const List<String> allFields = <String>[id, randomNumber];

  static const String id = 'id';

  static const String randomNumber = 'randomNumber';
}
