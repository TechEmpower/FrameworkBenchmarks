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
    var argResults = argParser.parse(args);
    serverMain(new StartConfig(0, argResults));

    for (int i = 1; i < Platform.numberOfProcessors; i++) {
      Isolate.spawn(serverMain, new StartConfig(i, argResults));
    }
  } on ArgParserException catch (e) {
    stderr
      ..writeln('fatal error: ${e.message}')
      ..writeln()
      ..writeln('usage: bin/main.dart [options...]')
      ..writeln(argParser.usage);
    exitCode = 1;
  }
}

void serverMain(StartConfig config) {
  var app = new Angel();

  app.configure(dart_angel_benchmark.configureServer(config.argResults)).then((_) async {
    var http = new AngelHttp.custom(app, startShared);
    var server = await http.startServer('127.0.0.1', 8080);
    var url = new Uri(
        scheme: 'http', host: server.address.address, port: server.port);
    print('Instance #${config.id} listening at $url');
  });
}

class StartConfig {
  final int id;
  final ArgResults argResults;

  StartConfig(this.id, this.argResults);
}