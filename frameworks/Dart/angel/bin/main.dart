import 'dart:async';
import 'dart:io';
import 'dart:isolate';
import 'package:angel_framework/angel_framework.dart';
import 'package:args/args.dart';
import 'package:dart_angel_benchmark/dart_angel_benchmark.dart'
    as dart_angel_benchmark;

main(List<String> args) async {
  var argParser = ArgParser()
    ..addOption('type',
        abbr: 't', allowed: ['mongo', 'postgres'], defaultsTo: 'mongo');

  try {
    var argResults = argParser.parse(args);
    serverMain(StartConfig(0, argResults));

    for (int i = 1; i < Platform.numberOfProcessors; i++) {
      var onError = new ReceivePort();
      onError.first.then((data) {
        print(data);

        if (data is List) {
          Zone.current.errorCallback(data[0], data[1] as StackTrace);
        }
      });
      Isolate.spawn(serverMain, StartConfig(i, argResults),
          onError: onError.sendPort);
    }
  } on ArgParserException catch (e) {
    stderr
      ..writeln('fatal error: ${e.message}')
      ..writeln('usage: bin/main.dart [options...]')
      ..writeln()
      ..writeln(argParser.usage);
    exitCode = 1;
  }
}

void serverMain(StartConfig config) {
  var app = Angel(
      //logger: Logger('tfb'),
      );

  // hierarchicalLoggingEnabled = true;

  //app.logger.onRecord.listen((rec) {
  //  print(rec);
  //  if (rec.error != null) print(rec.error);
  //  if (rec.stackTrace != null) print(rec.stackTrace);
  //});

  app
      .configure(dart_angel_benchmark.configureServer(config.argResults))
      .then((_) async {
    var http = AngelHttp.custom(app, startShared);
    var server = await http.startServer('0.0.0.0', 8080);
    var url =
        Uri(scheme: 'http', host: server.address.address, port: server.port);
    print('Instance #${config.id} listening at $url');
  });
}

class StartConfig {
  final int id;
  final ArgResults argResults;

  StartConfig(this.id, this.argResults);
}
