import 'dart:async';
import 'dart:math';
import 'package:angel3_framework/angel3_framework.dart';
import 'package:angel3_orm/angel3_orm.dart';
import '../../models/fortune.dart';
import '../../models/world.dart';

Future configureServer(Angel app) async {
  /// Controllers will not function unless wired to the application!

  var executor = app.container.make<QueryExecutor>();

  // Generate a random number between 1 and 10000
  int genRandomId() {
    var rand = Random();
    return rand.nextInt(10000) + 1;
  }

  int parseQueryCount(String? count) {
    if (count == null) {
      return 1;
    }

    var limit = int.tryParse(count) ?? 0;
    if (limit < 1) return 1;

    if (limit > 500) return 500;

    return limit;
  }

  List<int> generateIds(int maxCount) {
    var result = <int>[];

    while (result.length < maxCount) {
      var id = genRandomId();
      if (!result.contains(id)) {
        result.add(id);
      }
    }

    return result;
  }

  // Return data in json
  app.get('/json', (req, res) => res.json({'message': 'Hello, World!'}));

  const reply = "Hello, World!";

  // Return data in plaintext
  app.get('/plaintext', (req, res) async {
    res.contentLength = reply.length;
    res.write(reply);
  });

  // Add an entry and sort a list of fortune
  app.get('/fortunes', (req, res) async {
    var list = await FortuneQuery().get(executor);

    list.add(
        Fortune(id: 0, message: 'Additional fortune added at request time.'));
    list.sort((a, b) => a.message?.compareTo(b.message ?? '') ?? 0);

    res.render('listing', {'fortunes': list});
  });

  // Find a random World
  app.get('/db', (req, res) async {
    var id = genRandomId();
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

    var queryLimit = parseQueryCount(params['queries'] as String?);

    var list = generateIds(queryLimit);
    var result = <World>[];
    for (var id in list) {
      var query = WorldQuery();
      query.where?.id.equals(id);
      var optWorld = await query.getOne(executor);
      result.add(optWorld.value);
    }

    res.json(result);
  });

  // Update a list of worlds
  app.get('/updates', (req, res) async {
    var params = req.queryParameters;
    var queryLimit = parseQueryCount(params['queries'] as String?);
    var listOfIds = generateIds(queryLimit);

    var result = <World>[];
    for (var id in listOfIds) {
      var query = WorldQuery();
      query.where?.id.equals(id);
      var optWorld = await query.getOne(executor);

      query
        ..where?.id.equals(optWorld.value.id!)
        ..values.randomNumber = genRandomId();
      var updatedRec = await query.updateOne(executor);
      result.add(updatedRec.value);
    }

    res.json(result);
  });
}
