import 'dart:async';
import 'dart:math';
import 'package:angel_configuration/angel_configuration.dart';
import 'package:angel_framework/angel_framework.dart';
import 'package:args/args.dart';
import 'package:file/local.dart';
import 'package:mongo_dart/mongo_dart.dart';
import 'package:mustache4dart/mustache4dart.dart' as mustache;
import 'src/models/models.dart';
import 'src/query/query.dart';

AngelConfigurer configureServer(ArgResults argResults) {
  var rnd = new Random();
  var minQueryCount = 1;
  var maxQueryCount = 500;
  var worldTableSize = 10000;
  var fs = const LocalFileSystem();

  return (Angel app) async {
    // Load configuration.
    await app.configure(configuration(fs));

    // Set up the view engine.
    var fortunesTemplate =
        await fs.file('views/fortune.mustache').readAsString();

    app.viewGenerator =
        (name, [data]) => mustache.render(fortunesTemplate, data);

    // Select a querier, either MongoDB or PostgreSQL.
    //
    // Either way, the container *must* contain a `Querier`.
    if (argResults['type'] == 'mongo') {
      var db = Db(app.configuration['mongo_db']);
      app.container.registerSingleton<Querier>(MongoQuerier(db));
      await db.open();
      app.shutdownHooks.add((_) => db.close());
    } else {
      throw UnsupportedError('Unsupported DB ${argResults['type']}');
    }

    // JSON response.
    app.get('/json', (req, res) {
      res.serialize({'message': 'Hello, World!'});
    });

    // Plaintext response.
    app.get('/plaintext', (req, res) {
      res
        ..write('Hello, World!')
        ..close();
    });

    // Fetch random world object.
    app.get('/db', (req, res) async {
      var querier = req.container.make<Querier>();
      res.serialize(await querier.getRandomWorld());
    });

    // DB queries
    app.get('/queries/int:queryCount?', (req, res) async {
      // Get the querier and query count.
      var querier = req.container.make<Querier>();
      var queryCount = req.params['queryCount'] as int ?? minQueryCount;
      queryCount = queryCount.clamp(minQueryCount, maxQueryCount);

      // Fetch the objects.
      var worlds = await Future.wait<World>(
          List.generate(queryCount, (_) => querier.getRandomWorld()));
      res.serialize(worlds);
    });

    // DB updates
    app.get('/updates/int:queryCount?', (req, res) async {
      // Get the querier and query count.
      var querier = req.container.make<Querier>();
      var queryCount = req.params['queryCount'] as int ?? minQueryCount;
      queryCount = queryCount.clamp(minQueryCount, maxQueryCount);

      // Fetch the objects.
      var worlds =
          await Future.wait<World>(List.generate(queryCount, (_) async {
        var world = await querier.getRandomWorld();
        await querier.updateWorld(world.id,
            world.copyWith(randomNumber: rnd.nextInt(worldTableSize) + 1));
      }));
      res.serialize(worlds);
    });

    // Templating
    app.get('/fortunes', (req, res) async {
      var querier = req.container.make<Querier>();
      var fortunes = await querier.getFortunes();

      // Insert an additional fortune.
      fortunes.add(
        Fortune(
          id: 0,
          message: 'Additional fortune added at request time.',
        ),
      );

      // Sort the fortunes.
      fortunes.sort((a, b) => a.id.compareTo(b.id));

      // Render the template.
      await res.render('fortunes', {'fortunes': fortunes});
    });
  };
}
