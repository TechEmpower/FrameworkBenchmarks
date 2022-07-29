import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart';
import 'package:shelf_router/shelf_router.dart';

const defaultPort = 8080;

enum Headers {
  plainText({'Content-Type': 'text/plain'}),
  json({'Content-Type': 'text/json'});

  const Headers(this.value);
  final Map<String, Object> value;
}

enum Routes {
  plainText('/plainText'),
  json('/json');

  const Routes(this.value);
  final String value;
}

void main() {
  final errorPort = ReceivePort()..listen((e) => print(e));
  final exitPort = ReceivePort()..listen((_) => print('Server stopped'));

  for (var i = 0; i < Platform.numberOfProcessors; ++i) {
    Isolate.spawn(
      server,
      [],
      onError: errorPort.sendPort,
      onExit: exitPort.sendPort,
    );
  }
}

Router get router => Router()
  ..get(Routes.plainText.value, plainText)
  ..get(Routes.json.value, json);

Handler get handler =>
    Pipeline().addMiddleware(logRequests()).addHandler(router);

Future<void> server(List<void> _) async {
  final server = await serve(
    handler,
    InternetAddress.anyIPv4,
    defaultPort,
    shared: true,
  );
  print('Server listening on port ${server.port}');
}

Response plainText(Request req) => Response.ok(
      'Hello, World!',
      headers: Headers.plainText.value,
    );

Response json(Request request) => Response.ok(
      jsonEncode({'message': 'Hello, World!'}),
      headers: Headers.json.value,
    );
