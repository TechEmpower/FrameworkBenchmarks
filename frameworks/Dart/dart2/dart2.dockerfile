FROM dart:2.17.6

WORKDIR /dart_app
COPY pubspec.yaml pubspec.yaml
COPY server.dart server.dart

RUN dart pub upgrade

EXPOSE 8080

CMD ["dart", "server.dart"]
