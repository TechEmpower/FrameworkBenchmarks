import 'dart:io';

import 'package:dart_frog/dart_frog.dart';

Response onRequest(RequestContext context) {
  return Response.json(
    body: {'message': 'Hello, World!'},
    headers: {
      HttpHeaders.dateHeader: '${HttpDate.format(DateTime.now())}',
      HttpHeaders.serverHeader: 'dart_frog',
    },
  );
}
