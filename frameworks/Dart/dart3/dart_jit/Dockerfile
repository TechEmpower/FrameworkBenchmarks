
FROM dart:3.11.0 AS build
WORKDIR /app

COPY dart_jit/pubspec.yaml .
COPY dart_jit/bin/ bin/

EXPOSE 8080
ENTRYPOINT ["dart", "run", "bin/server.dart"]