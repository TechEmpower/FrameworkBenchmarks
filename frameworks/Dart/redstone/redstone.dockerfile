FROM google/dart:1.24

WORKDIR /redstone
COPY fortunes.mustache fortunes.mustache
COPY mongodb.yaml mongodb.yaml
COPY postgresql.yaml postgresql.yaml
COPY pubspec.yaml pubspec.yaml
COPY server.dart server.dart

RUN pub upgrade

CMD ["dart", "server.dart"]
