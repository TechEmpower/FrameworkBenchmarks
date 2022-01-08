/// Custom plugins go here.
import 'dart:async';
import 'package:angel3_framework/angel3_framework.dart';
import 'orm.dart' as orm;

Future configureServer(Angel app) async {
  // Include any plugins you have made here.

  await app.configure(orm.configureServer);
}
