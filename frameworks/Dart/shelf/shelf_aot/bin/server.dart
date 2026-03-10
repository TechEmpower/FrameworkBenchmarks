import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:math' show min;

import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;

/// Environment declarations are evaluated at compile-time. Use 'const' to
/// ensure values are baked into AOT/Native binaries for the benchmark.
///
/// From https://api.dart.dev/dart-core/int/int.fromEnvironment.html:
/// "This constructor is only guaranteed to work when invoked as const.
/// It may work as a non-constant invocation on some platforms
/// which have access to compiler options at run-time,
/// but most ahead-of-time compiled platforms will not have this information."
const _maxIsolatesfromEnvironment = int.fromEnvironment('MAX_ISOLATES');

/// The fixed TCP port used by the server.
/// Defined here for visibility and ease of configuration.
const _defaultPort = 8080;

/// A reusable instance of the UTF-8 JSON encoder to efficiently
/// transform Dart objects into byte arrays for HTTP responses.
final _jsonEncoder = JsonUtf8Encoder();

/// Internal token used to notify newly spawned processes that they
/// belong to a secondary "worker group".
const _workerGroupTag = '--WORKER-GROUP';

void main(List<String> arguments) async {
  /// Create a mutable copy of the fixed-length arguments list.
  final args = [...arguments];

  /// Defines local isolate quota, using MAX_ISOLATES if provided.
  /// Falls back to total available cores while respecting hardware limits.
  var maxIsolates = _maxIsolatesfromEnvironment > 0
      ? min(_maxIsolatesfromEnvironment, Platform.numberOfProcessors)
      : Platform.numberOfProcessors;

  /// Determine if this process instance was initialized as a worker group.
  if (args.contains(_workerGroupTag)) {
    /// Sanitize the argument list to ensure the internal token does not
    /// interfere with application-level argument parsing.
    args.remove(_workerGroupTag);
  }
  /// Prevents recursive spawning
  /// by ensuring only the primary process can spawn worker groups
  else {
    /// Calculate the number of secondary worker groups required
    /// to fully utilize the available hardware capacity.
    ///
    /// Each group serves as a container for multiple isolates,
    /// helping to bypass internal VM scaling bottlenecks.
    final workerGroups = Platform.numberOfProcessors ~/ maxIsolates - 1;

    /// Bootstraps independent worker processes via AOT snapshots.
    /// Each process initializes its own Isolate Group.
    for (var i = 0; i < workerGroups; i++) {
      /// [Platform.script] identifies the AOT snapshot or executable.
      /// [Isolate.spawnUri] spawns a new process group via [main()].
      await Isolate.spawnUri(Platform.script, [...args, _workerGroupTag], null);
    }

    /// Updates local isolate limits, assigning the primary group
    /// the remaining cores after worker group allocation.
    maxIsolates = Platform.numberOfProcessors - workerGroups * maxIsolates;
  }

  /// Create an [Isolate] containing an [HttpServer]
  /// for each processor after the first
  for (var i = 1; i < maxIsolates; i++) {
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
    _defaultPort,
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
      HttpHeaders.dateHeader: HttpDate.format(DateTime.now()),
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
