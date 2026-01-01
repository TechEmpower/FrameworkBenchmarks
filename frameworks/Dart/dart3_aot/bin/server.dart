import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

/// The maximum number of isolates managed by a single OS process group.
const _isolatesPerGroup = 8;

/// Internal token used to notify newly spawned processes that they
/// belong to a secondary "worker group".
const _workerGroupTag = '--workerGroup';

void main(List<String> args) async {
  /// Determine if this process instance was initialized as a worker group.
  final isWorkerGroup = args.contains(_workerGroupTag);
  if (isWorkerGroup) {
    /// Sanitize the argument list to ensure the internal token does not
    /// interfere with application-level argument parsing.
    args.removeAt(args.indexOf(_workerGroupTag));
  }

  /// Calculate the number of secondary worker groups required
  /// to fully utilize the available hardware capacity.
  ///
  /// Each group serves as a container for multiple isolates,
  /// helping to bypass internal VM scaling bottlenecks.
  final workerGroups = Platform.numberOfProcessors ~/ _isolatesPerGroup - 1;
  if (!isWorkerGroup) {
    for (var i = 0; i < workerGroups; i++) {
      /// [Platform.script] identifies the AOT snapshot or executable.
      /// [Isolate.spawnUri] bootstraps an entirely new group by re-executing [main()].
      Isolate.spawnUri(Platform.script, [_workerGroupTag], null);
    }
  }

  /// Determine the isolate quota for the current group.
  /// Secondary worker groups run a full set defined by [_isolatesPerGroup];
  /// the primary group manages the remaining available cores.
  final currentGroupIsolates = isWorkerGroup
      ? _isolatesPerGroup
      : Platform.numberOfProcessors - workerGroups * _isolatesPerGroup;

  /// Create an [Isolate] for the "Local Group" containing an [HttpServer]
  /// for each processor available in this group after the first
  for (var i = 1; i < currentGroupIsolates; i++) {
    await Isolate.spawn(_startServer, args);
  }

  /// Initialize the server instance for the group's lead isolate.
  await _startServer(args);
}

/// Creates and setup a [HttpServer]
Future<void> _startServer(List<String> args) async {
  /// Binds the [HttpServer] on `0.0.0.0:8080`.
  final server = await HttpServer.bind(
    InternetAddress.anyIPv4,
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
void _jsonTest(HttpRequest request) =>
    _sendJson(request, const {'message': 'Hello, World!'});

/// Responds with the plaintext test to the [request].
void _plaintextTest(HttpRequest request) => _sendText(request, 'Hello, World!');

final _jsonEncoder = JsonUtf8Encoder();
