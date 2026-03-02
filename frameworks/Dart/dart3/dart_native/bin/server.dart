import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

/// The fixed TCP port used by the server.
/// Defined here for visibility and ease of configuration.
const _defaultPort = 8080;

/// A reusable instance of the UTF-8 JSON encoder to efficiently
/// transform Dart objects into byte arrays for HTTP responses.
final _jsonEncoder = JsonUtf8Encoder();

void main(List<String> args) async {
  /// Create an [Isolate] containing an [HttpServer]
  /// for each processor after the first
  for (var i = 1; i < Platform.numberOfProcessors; i++) {
    await Isolate.spawn(_startServer, args);
  }

  /// Create a [HttpServer] for the first processor
  await _startServer(args);
}

/// Creates and setup a [HttpServer]
Future<void> _startServer(List<String> args) async {
  /// Binds the [HttpServer] on `0.0.0.0:8080`.
  final server = await HttpServer.bind(
    InternetAddress.anyIPv4,
    _defaultPort,
    shared: true,
  );

  server
    ..defaultResponseHeaders.clear()
    /// Sets [HttpServer]'s [serverHeader].
    ..serverHeader = 'dart_native'
    /// Handles [HttpRequest]'s from [HttpServer].
    ..listen(_handleRequest);
}

/// Dispatches requests to specific test handlers. Wrapped in a try-catch
/// to ensure stable execution and guaranteed response delivery.
Future<void> _handleRequest(HttpRequest request) async {
  try {
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
  } catch (e) {
    _sendResponse(request, HttpStatus.internalServerError);
  }
}

/// Completes the given [request] by writing the [bytes] with the given
/// [statusCode] and [type].
void _sendResponse(
  HttpRequest request,
  int statusCode, {
  ContentType? type,
  List<int> bytes = const [],
}) => request.response
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
  bytes: _jsonEncoder.convert(response),
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
