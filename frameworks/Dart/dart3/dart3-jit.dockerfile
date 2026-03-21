
FROM dart:3.11.0 AS build
WORKDIR /app

COPY dart_jit/ .

EXPOSE 8080
ENTRYPOINT ["dart", "run", "bin/server.dart"]