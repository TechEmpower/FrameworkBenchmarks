FROM google/dart:2.12

WORKDIR /dart_app
COPY pubspec.yaml pubspec.yaml
COPY server.dart server.dart

RUN pub upgrade

EXPOSE 8080

CMD ["dart", "server.dart"]
