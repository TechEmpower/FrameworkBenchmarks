import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:math' show min;
import 'dart:typed_data';

import 'package:relic/relic.dart';

/// Fetches the 'MAX_ISOLATES' compile-time configuration.
const _maxIsolatesfromEnvironment = int.fromEnvironment('MAX_ISOLATES');

/// Internal token used to notify newly spawned processes.
const _workerGroupTag = '--WORKER-GROUP';

/// The fixed TCP port used by the server.
const _defaultPort = 8080;

/// Reusable UTF-8 JSON encoder for efficient byte array transformations.
final _jsonEncoder = JsonUtf8Encoder();

void main(List<String> args) async {
  /// Defines local isolate quota, using MAX_ISOLATES if provided.
  var maxIsolates = _maxIsolatesfromEnvironment > 0
      ? min(_maxIsolatesfromEnvironment, Platform.numberOfProcessors)
      : Platform.numberOfProcessors;

  /// Determine if this process instance was initialized as a worker group.
  if (!args.contains(_workerGroupTag)) {
    /// Calculate the number of secondary worker groups required
    final workerGroups = Platform.numberOfProcessors ~/ maxIsolates - 1;

    /// Bootstraps independent worker processes via AOT snapshots.
    for (var i = 0; i < workerGroups; i++) {
      await Isolate.spawnUri(Platform.script, [...args, _workerGroupTag], null);
    }

    /// Updates local isolate limits, assigning the primary group
    /// the remaining cores after worker group allocation.
    maxIsolates = Platform.numberOfProcessors - workerGroups * maxIsolates;
  }

  /// Entry point: starts the server utilizing available CPU cores.
  await _app.serve(
    address: InternetAddress.anyIPv4,
    port: _defaultPort,
    noOfIsolates: maxIsolates,
    shared: true, // Allows isolates to share the same port.
  );
}

/// Configures the Relic application routing and middleware.
final _app = RelicApp()
  ..use('/', _requiredHeadersMiddleware)
  ..get('/json', respondWith((request) => _responseJson()))
  ..get('/plaintext', respondWith((request) => _responsePlainText()))
  ..fallback = respondWith((request) => Response.notFound());

/// Middleware to inject TFB-required [Server] and [Date] headers globally.
final _requiredHeadersMiddleware = createMiddleware(
  onResponse: (response) => response.copyWith(
    headers: response.headers.transform(
      (headers) => headers
        ..server = 'relic'
        ..date = DateTime.now(),
    ),
  ),
);

/// Handles the '/json' endpoint, returning a pre-encoded JSON response.
Response _responseJson() => Response.ok(
  body: Body.fromData(
    _jsonEncoder.convert(const {'message': 'Hello, World!'}) as Uint8List,
    mimeType: MimeType.json,
  ),
);

/// Handles the '/plaintext' endpoint, returning a simple text response.
Response _responsePlainText() => Response.ok(
  body: Body.fromString('Hello, World!', mimeType: MimeType.plainText),
);
