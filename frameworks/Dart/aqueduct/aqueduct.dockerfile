FROM google/dart:1.24

COPY ./ ./

RUN pub upgrade

CMD dart server.dart
