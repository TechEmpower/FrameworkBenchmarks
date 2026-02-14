import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:math' show min;
import 'package:args/args.dart' show ArgParser;

/// Environment declarations are evaluated at compile-time. Use 'const' to
/// ensure values are baked into AOT/Native binaries for the benchmark.
///
/// From https://api.dart.dev/dart-core/int/int.fromEnvironment.html:
/// "This constructor is only guaranteed to work when invoked as const.
/// It may work as a non-constant invocation on some platforms
/// which have access to compiler options at run-time,
/// but most ahead-of-time compiled platforms will not have this information."
const _maxIsolatesfromEnvironment = int.fromEnvironment('MAX_ISOLATES');

void main(List<String> args) {
  /// Defines local isolate quota, using MAX_ISOLATES if provided.
  /// Falls back to total available cores while respecting hardware limits.
  var maxIsolates = _maxIsolatesfromEnvironment > 0
      ? min(_maxIsolatesfromEnvironment, Platform.numberOfProcessors)
      : Platform.numberOfProcessors;

  /// Triggers process-level horizontal scaling when running in AOT.
  if (Platform.script.toFilePath().endsWith('.aot')) {
    /// Internal token used to notify newly spawned processes that they
    /// belong to a secondary "worker group".
    const workerGroupTag = '--workerGroup';

    /// Determine if this process instance was initialized as a worker group.
    final isWorkerGroup = args.contains(workerGroupTag);

    if (isWorkerGroup) {
      /// Sanitize the argument list to ensure the internal token does not
      /// interfere with application-level argument parsing.
      args.removeAt(args.indexOf(workerGroupTag));
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
        Isolate.spawnUri(Platform.script, [...args, workerGroupTag], null);
      }

      /// Updates local isolate limits, assigning the primary group
      /// the remaining cores after worker group allocation.
      maxIsolates = Platform.numberOfProcessors - workerGroups * maxIsolates;
    }
  }

  /// Create an [Isolate] containing an [HttpServer]
  /// for each processor after the first
  for (var i = 1; i < maxIsolates; i++) {
    Isolate.spawn(_startServer, args);
  }

  /// Create a [HttpServer] for the first processor
  _startServer(args);
}

/// Creates and setup a [HttpServer]
void _startServer(List<String> args) async {
  /// Binds the [HttpServer] on `0.0.0.0:8080`.
  final server = await HttpServer.bind(
    InternetAddress.anyIPv4,
    _portParser(args, defaultPort: 8080),
    shared: true,
  );

  server
    ..defaultResponseHeaders.clear()
    /// Sets [HttpServer]'s [serverHeader].
    ..serverHeader = 'dart'
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

final _jsonEncoder = JsonUtf8Encoder();

int _portParser(
  List<String> args, {
  required int defaultPort,
  portTag = 'port',
}) {
  final parser = ArgParser()
    ..addOption(
      portTag,
      defaultsTo: '$defaultPort',
    );
  return int.tryParse(parser.parse(args)[portTag]) ?? defaultPort;
}
