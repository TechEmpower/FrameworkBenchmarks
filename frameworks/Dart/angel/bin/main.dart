import 'dart:io';
import 'dart:isolate';
import 'package:angel_framework/angel_framework.dart';
import 'package:args/args.dart';
import 'package:dart_angel_benchmark/dart_angel_benchmark.dart'
    as dart_angel_benchmark;

main(List<String> args) async {
  var argParser = new ArgParser()
    ..addOption('type',
        abbr: 't', allowed: ['mongo', 'postgres'], defaultsTo: 'mongo');

  try {
    for (int i = 1; i < Platform.numberOfProcessors; i++) {
      Isolate.spawn(serverMain, i);
    }

    serverMain(0);
  } on ArgParserException catch (e) {
    stderr
      ..writeln('fatal error: ${e.message}')
      ..writeln()
      ..writeln('usage: bin/main.dart [options...]')
      ..writeln(argParser.usage);
    exitCode = 1;
  }
}

void serverMain(int id) {
  var app = new Angel();

  app.configure(dart_angel_benchmark.configureServer).then((_) async {
    var http = new AngelHttp.custom(app, startShared);
    var server = await http.startServer('127.0.0.1', 8080);
    var url = new Uri(
        scheme: 'http', host: server.address.address, port: server.port);
    print('Instance #$id listening at $url');
  });
}
