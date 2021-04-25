import 'dart:async' show Future;
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

final _procNumber = Platform.numberOfProcessors;

final _encoder = JsonUtf8Encoder();

void main(List<String> args) {
  var errorPort = ReceivePort();
  errorPort.listen((e) => print(e));
  for (var i = 1; i < _procNumber; i++) {
    Isolate.spawn(_startInIsolate, [], onError: errorPort.sendPort);
  }
  _startInIsolate([]);
}

void _startInIsolate(List args) {
  _startServer();
}

Future<void> _startServer() async {
  final server = await HttpServer.bind('0.0.0.0', 8080, shared: true);
  server.defaultResponseHeaders.clear();
  server.serverHeader = 'dart';
  server.listen((request) {
    switch (request.uri.path) {
      case '/json':
        _jsonTest(request);
        break;
      case '/plaintext':
        _plaintextTest(request);
        break;
      default:
        _sendResponse(request, HttpStatus.notFound);
        break;
    }
  });
}

/// Completes the given [request] by writing the [bytes] with the given
/// [statusCode] and [type].
void _sendResponse(
  HttpRequest request,
  int statusCode, {
  ContentType? type,
  List<int>? bytes,
}) {
  final response = request.response;
  response
    ..statusCode = statusCode
    ..headers.date = DateTime.now();
  if (type != null) {
    response.headers.contentType = type;
  }
  if (bytes != null) {
    response
      ..contentLength = bytes.length
      ..add(bytes);
  } else {
    response.contentLength = 0;
  }
  response.close();
}

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
void _jsonTest(HttpRequest request) =>
    _sendJson(request, const {'message': 'Hello, World!'});

/// Responds with the plaintext test to the [request].
void _plaintextTest(HttpRequest request) => _sendText(request, 'Hello, World!');
