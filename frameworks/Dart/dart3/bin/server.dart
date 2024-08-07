import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

final _encoder = JsonUtf8Encoder();

void main(List<String> _) async {
  /// Create an [Isolate] containinig an [HttpServer]
  /// for each processor after the first
  for (var i = 1; i < Platform.numberOfProcessors; i++) {
    await Isolate.spawn(_startServer, _);
  }

  /// Create a [HttpServer] for the first processor
  await _startServer(_);
}

/// Creates and setup a [HttpServer]
Future<void> _startServer(List<String> _) async {
  /// Binds the [HttpServer] on `0.0.0.0:8080`.
  final server = await HttpServer.bind(
    InternetAddress('0.0.0.0', type: InternetAddressType.IPv4),
    8080,
    shared: true,
  );

  /// Sets [HttpServer]'s [serverHeader].
  server
    ..defaultResponseHeaders.clear()
    ..serverHeader = 'dart';

  /// Handles [HttpRequest]'s from [HttpServer].
  await for (final request in server) {
    switch (request.uri.path) {
      case '/json':
        _jsonTest(request);
        break;
      case '/plaintext':
        _plaintextTest(request);
        break;
      default:
        _sendResponse(request, HttpStatus.notFound);
    }
  }
}

/// Completes the given [request] by writing the [bytes] with the given
/// [statusCode] and [type].
void _sendResponse(
  HttpRequest request,
  int statusCode, {
  ContentType? type,
  List<int> bytes = const [],
}) =>
    request.response
      ..statusCode = statusCode
      ..headers.contentType = type
      ..headers.date = DateTime.now()
      ..contentLength = bytes.length
      ..add(bytes)
      ..close();

/// Completes the given [request] by writing the [response] as JSON.
void _sendJson(HttpRequest request, Object response) => _sendResponse(
      request,
      HttpStatus.ok,
      type: ContentType.json,
      bytes: _encoder.convert(response),
    );

/// Completes the given [request] by writing the [response] as plain text.
void _sendText(HttpRequest request, String response) => _sendResponse(
      request,
      HttpStatus.ok,
      type: ContentType.text,
      bytes: utf8.encode(response),
    );

/// Responds with the JSON test to the [request].
void _jsonTest(HttpRequest request) => _sendJson(
      request,
      const {'message': 'Hello, World!'},
    );

/// Responds with the plaintext test to the [request].
void _plaintextTest(HttpRequest request) => _sendText(
      request,
      'Hello, World!',
    );
