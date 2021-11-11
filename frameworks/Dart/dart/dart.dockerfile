FROM google/dart:1.24

WORKDIR /dart_app
COPY fortunes.mustache fortunes.mustache
COPY postgresql.yaml postgresql.yaml
COPY pubspec.yaml pubspec.yaml
COPY server.dart server.dart

RUN pub upgrade

EXPOSE 8080

CMD ["dart", "server.dart"]
