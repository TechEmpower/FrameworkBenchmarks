
FROM dart:3.11.0 AS build
WORKDIR /app

COPY pubspec.yaml .
COPY dart_hybrid/bin/ bin/

RUN dart compile exe bin/server.dart -o server

FROM scratch
COPY --from=build /runtime/ /
COPY --from=build /app/server /bin/server

EXPOSE 8080
ENTRYPOINT ["server"]