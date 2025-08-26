import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;

void main(List<String> args) async {
  /// Create an [Isolate] containing an [HttpServer]
  /// for each processor after the first
  for (var i = 1; i < Platform.numberOfProcessors; i++) {
    await Isolate.spawn(_startServer, args);
  }

  /// Create a [HttpServer] for the first processor
  await _startServer(args);
}

/// Create a request handler
Response _handler(Request request) {
  switch (request.url.path) {
    case 'json':
      return _jsonTest();
    case 'plaintext':
      return _plaintextTest();
    default:
      return _sendResponse(HttpStatus.notFound);
  }
}

/// Creates and setup a [HttpServer].
Future<void> _startServer(List<String> _) async {
  final server = await shelf_io.serve(
    _handler,
    InternetAddress.anyIPv4,
    8080,
    shared: true,
  );

  /// Sets [HttpServer]'s [serverHeader].
  server
    ..defaultResponseHeaders.clear()
    ..serverHeader = 'shelf';
}

/// Completes the given [request] by writing the [bytes] with the given
/// [statusCode] and [type].
Response _sendResponse(
  int statusCode, {
  ContentType? type,
  List<int> bytes = const [],
}) {
  return Response(
    statusCode,
    headers: {
      HttpHeaders.contentLengthHeader: '${bytes.length}',
      HttpHeaders.dateHeader: '${HttpDate.format(DateTime.now())}',
      if (type != null) HttpHeaders.contentTypeHeader: type.mimeType,
    },
    body: bytes,
  );
}

/// Completes the given [request] by writing the [response] as JSON.
Response _sendJson(Object response) => _sendResponse(
  HttpStatus.ok,
  type: ContentType.json,
  bytes: _jsonEncoder.convert(response),
);

/// Completes the given [request] by writing the [response] as plain text.
Response _sendText(String response) => _sendResponse(
  HttpStatus.ok,
  type: ContentType.text,
  bytes: utf8.encode(response),
);

/// Responds with the JSON test to the [request].
Response _jsonTest() => _sendJson(const {'message': 'Hello, World!'});

/// Responds with the plaintext test to the [request].
Response _plaintextTest() => _sendText('Hello, World!');

final _jsonEncoder = JsonUtf8Encoder();
