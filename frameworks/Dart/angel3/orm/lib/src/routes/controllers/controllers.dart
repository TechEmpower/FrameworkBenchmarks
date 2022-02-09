import 'dart:async';
import 'dart:math';
import 'package:angel3_framework/angel3_framework.dart';
import 'package:angel3_orm/angel3_orm.dart';
import '../../models/fortune.dart';
import '../../models/world.dart';

Future configureServer(Angel app) async {
  /// Controllers will not function unless wired to the application!

  var executor = app.container!.make<QueryExecutor>();

  // Generate a random number between 1 and 10000
  int _genRandomId() {
    var rand = Random();
    return rand.nextInt(10000) + 1;
  }

  int _parseQueryCount(String? count) {
    if (count == null) {
      return 1;
    }

    var limit = int.tryParse(count) ?? 0;
    if (limit < 1) return 1;

    if (limit > 500) return 500;

    return limit;
  }

  List<int> _generateIds(int maxCount) {
    var result = <int>[];

    while (result.length < maxCount) {
      var id = _genRandomId();
      if (!result.contains(id)) {
        result.add(id);
      }
    }

    return result;
  }

  // Return data in json
  app.get('/json', (req, res) => res.json({'message': 'Hello, World!'}));

  // Return data in plaintext
  app.get('/plaintext', (req, res) async {
    res.write('Hello, World!');
    res.close();
  });

  // Add an entry and sort a list of fortune
  app.get('/fortunes', (req, res) async {
    //var stopwatch = Stopwatch()..start();

    var list = await FortuneQuery().get(executor);

    //print('Query Time: ${stopwatch.elapsed.inMilliseconds}ms');

    list.add(
        Fortune(id: 0, message: 'Additional fortune added at request time.'));
    list.sort((a, b) => a.message?.compareTo(b.message ?? '') ?? 0);

    //print('Process Time: ${stopwatch.elapsed.inMilliseconds}ms');
    //stopwatch.stop();

    //res.json(list);
    res.render('listing', {'fortunes': list});
  });

  // Find a random World
  app.get('/db', (req, res) async {
    var id = _genRandomId();
    var query = WorldQuery()..where?.id.equals(id);
    var result = await query.get(executor);
    if (result.isNotEmpty) {
      res.json(result[0]);
    } else {
      res.json({});
    }
  });

  // Return a list of worlds
  app.get('/query', (req, res) async {
    var params = req.queryParameters;

    var queryLimit = _parseQueryCount(params['queries'] as String?);

    var list = _generateIds(queryLimit);
    var query = WorldQuery();
    var result = <World>[];
    for (var id in list) {
      query.where?.id.equals(id);
      var optWorld = await query.getOne(executor);
      result.add(optWorld.value);
    }

    res.json(result);
  });

  // Update a list of worlds
  app.get('/updates', (req, res) async {
    //var stopwatch = Stopwatch()..start();

    var params = req.queryParameters;
    var queryLimit = _parseQueryCount(params['queries'] as String?);
    var listOfIds = _generateIds(queryLimit);

    var query = WorldQuery();
    var result = <World>[];
    for (var id in listOfIds) {
      query.where?.id.equals(id);
      var optWorld = await query.getOne(executor);

      query
        ..where?.id.equals(optWorld.value.id!)
        ..values.randomNumber = _genRandomId();
      var updatedRec = await query.updateOne(executor);
      result.add(updatedRec.value);
    }

    //rint('Process Time: ${stopwatch.elapsed.inMilliseconds}ms');
    //stopwatch.stop();

    res.json(result);
  });
}
