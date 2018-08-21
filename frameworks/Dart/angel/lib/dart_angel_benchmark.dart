import 'package:angel_configuration/angel_configuration.dart';
import 'package:angel_framework/angel_framework.dart';
import 'package:args/args.dart';
import 'package:file/local.dart';
import 'package:mongo_dart/mongo_dart.dart';
import 'src/query/query.dart';

AngelConfigurer configureServer(ArgResults argResults) {
  return (Angel app) async {
    // Load configuration.
    var fs = const LocalFileSystem();
    await app.configure(configuration(fs));

    // Select a querier, either MongoDB or PostgreSQL.
    //
    // Either way, the container *must* contain a `Querier`.
    if (argResults['type'] == 'mongo') {
      var db = Db(app.configuration['mongo_db']);
      app.container.registerSingleton<Querier>(MongoQuerier(db));
      await db.open();
    } else {
      throw UnsupportedError('Unsupported DB ${argResults['type']}');
    }
  };
}
