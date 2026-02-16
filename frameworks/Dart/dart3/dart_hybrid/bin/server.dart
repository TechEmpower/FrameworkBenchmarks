import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

void main() {
  /// Retrieve the maximum number of isolates from the environment variables.
  final maxIsolatesFromEnvironment = Platform.environment['MAX_ISOLATES'];
  if (maxIsolatesFromEnvironment == null) {
    throw Exception('MAX_ISOLATES environment variable is not set.');
  }

  /// Parse the isolate count; ensures we have a valid integer for scaling.
  final maxIsolates = int.tryParse(maxIsolatesFromEnvironment);
  if (maxIsolates == null) {
    throw Exception('MAX_ISOLATES must be a valid integer.');
  }

  /// Retrieve the file system path for the Unix Domain Socket.
  final socketPath = Platform.environment['SOCKET_PATH'];
  if (socketPath == null) {
    throw Exception('SOCKET_PATH environment variable is not set.');
  }

  /// Define the network address as a Unix Domain Socket (UDS).
  /// This is often faster than TCP for local inter-process communication.
  final address = InternetAddress(socketPath, type: InternetAddressType.unix);

  /// Create an [Isolate] containing an [HttpServer]
  /// for each processor after the first
  for (int i = 1; i < maxIsolates; i++) {
    Isolate.spawn<InternetAddress>(_startServer, address);
  }

  /// Create a [HttpServer] for the first processor
  _startServer(address);
}

/// Initializes and binds the [HttpServer] to the provided [address].
///
/// Setting [shared] to true allows multiple isolates to bind to the same
/// address/port, enabling automatic load balancing by the OS kernel.
void _startServer(InternetAddress address) async {
  final server = await HttpServer.bind(address, 0, shared: true);

  server
    ..defaultResponseHeaders.clear()
    /// Sets [HttpServer]'s [serverHeader].
    ..serverHeader = 'dart_hybrid'
    /// Handles [HttpRequest]'s from [HttpServer].
    ..listen(_handleRequest);
}

/// Dispatches requests to specific handlers.
void _handleRequest(HttpRequest request) {
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
  bytes: JsonUtf8Encoder().convert(response),
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
