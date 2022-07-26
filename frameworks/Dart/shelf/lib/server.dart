import 'dart:async';
import 'dart:io';

import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart';
import 'package:shelf_router/shelf_router.dart';

import 'src/json.dart';
import 'src/multithread.dart';
import 'src/plain_text.dart';

const defaultPort = 8080;
const defaultAddress = '0.0.0.0';

void main(List<String> args) => runZonedGuarded(
      () => multithread(
        entryPoint: entryPoint,
        pipeline: pipeline(
          {
            '/plainText': plainText,
            '/json': json,
          },
        ),
      ),
      (e, s) => print('$e,$s'),
    );

Service pipeline(Map<String, Service> services) {
  final router = Router();
  for (final service in services.entries) {
    router.get(service.key, service.value);
  }
  return const Pipeline().addMiddleware(logRequests()).addHandler(router);
}

Future<HttpServer> entryPoint(Service pipeline) => serve(
      pipeline,
      defaultAddress,
      defaultPort,
      shared: true,
    );
