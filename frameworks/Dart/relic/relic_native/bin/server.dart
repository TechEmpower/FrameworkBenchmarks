import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:relic/relic.dart';

/// The fixed TCP port used by the server.
const _defaultPort = 8080;

/// Reusable UTF-8 JSON encoder for efficient byte array transformations.
final _jsonEncoder = JsonUtf8Encoder();

/// Entry point: starts the server utilizing all available CPU cores.
void main() async {
  await _app.serve(
    address: InternetAddress.anyIPv4,
    port: _defaultPort,
    noOfIsolates: Platform.numberOfProcessors,
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
