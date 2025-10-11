import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:typed_data';

import 'package:relic/io_adapter.dart';
import 'package:relic/relic.dart';

void main() async {
  /// Number of [Isolate]s to spawn
  /// This is based on the number of available processors
  /// minus one for the main isolate
  final isolateCount = Platform.numberOfProcessors * 2 - 1;

  /// Create an [Isolate] containing an [HttpServer]
  await Future.wait(
    List.generate(
      isolateCount,
      (final index) =>
          Isolate.spawn((final _) => _serve(), null, debugName: '$index'),
    ),
  );

  _serve();
}

/// [_serve] is called in each spawned isolate.
Future<void> _serve() async {
  final router = Router<Handler>()
    ..get('/json', respondWith((req) => _responseJson()))
    ..get('/plaintext', respondWith((req) => _responsePlainText()));

  final handler = const Pipeline()
      .addMiddleware(_requiredHeadersMiddleware())
      .addMiddleware(routeWith(router))
      .addHandler(respondWith((_) => Response.notFound()));

  // start the server
  await serve(handler, InternetAddress.anyIPv4, 8080, shared: true);
}

Middleware _requiredHeadersMiddleware() {
  var addHeaders = createMiddleware(
    onResponse: (response) => response.copyWith(
      headers: response.headers.transform((headers) {
        headers.server = 'relic';
        // Date header is added by default, but we set it here for clarity.
        headers.date = DateTime.now();
      }),
    ),
  );
  return addHeaders;
}

Response _responseJson() {
  return Response.ok(
    body: Body.fromData(
      _jsonEncoder.convert(const {'message': 'Hello, World!'}) as Uint8List,
      mimeType: MimeType.json,
    ),
  );
}

Response _responsePlainText() {
  return Response.ok(
    body: Body.fromString('Hello, World!', mimeType: MimeType.plainText),
  );
}

final _jsonEncoder = JsonUtf8Encoder();
