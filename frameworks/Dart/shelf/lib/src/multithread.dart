import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:shelf/shelf.dart';

typedef Service = FutureOr<Response> Function(Request);
typedef EntryPoint<T> = void Function(T);

Future<List<Isolate>> multithread({
  required EntryPoint<Service> entryPoint,
  required Service pipeline,
}) =>
    Future.wait([
      for (var i = 0; i < Platform.numberOfProcessors; ++i)
        Isolate.spawn<Service>(
          entryPoint,
          pipeline,
        ),
    ]);
