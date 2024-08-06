import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

final _numberOfProcessors = Platform.numberOfProcessors;

final _encoder = JsonUtf8Encoder();

void main(List<String> _) async {
  final errorPort = ReceivePort()..listen((e) => print(e));
  for (var i = 1; i < _numberOfProcessors; i++) {
    await Isolate.spawn(_startInIsolate, [], onError: errorPort.sendPort);
  }
  _startInIsolate([]);
}

void _startInIsolate(List _) =>
    HttpServer.bind('0.0.0.0', 8080, shared: true).then(
      (server) => server
        ..defaultResponseHeaders.clear()
        ..serverHeader = 'dart'
        ..listen(_onHttpRequest),
    );

void _onHttpRequest(HttpRequest request) {
  switch (request.uri.path) {
    case '/json':
      _jsonTest(request);
      break;
    case '/plaintext':
      _plaintextTest(request);
      break;
    default:
      _sendResponse(
        request,
        HttpStatus.notFound,
        type: UnsupportedContentType(),
      );
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

final class UnsupportedContentType implements ContentType {
  static const _message = 'ContentType not supported';
  @override
  String? get charset => throw Exception(_message);
  @override
  String get mimeType => throw Exception(_message);
  @override
  Map<String, String?> get parameters => throw Exception(_message);
  @override
  String get primaryType => throw Exception(_message);
  @override
  String get subType => throw Exception(_message);
  @override
  String get value => throw Exception(_message);
}
