import '../dart_aqueduct_benchmark.dart';

class Model extends ManagedObject<_Model> implements _Model {
  @override
  void willInsert() {
    createdAt = new DateTime.now().toUtc();
  }
}

class _Model {
  @managedPrimaryKey
  int id;


  @ManagedColumnAttributes(indexed: true)
  String name;

  DateTime createdAt;
}
