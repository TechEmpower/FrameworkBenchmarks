import 'dart:io';
import 'package:dart_frog/dart_frog.dart';

Response onRequest(RequestContext context) {
  return Response(
    body: 'Hello, World!',
    headers: {
      HttpHeaders.contentTypeHeader: ContentType.text.mimeType,
      HttpHeaders.dateHeader: '${HttpDate.format(DateTime.now())}',
      HttpHeaders.serverHeader: 'dart_frog',
    },
  );
}
