FROM google/dart:1.24

COPY ./ ./

RUN pub upgrade

EXPOSE 8080

CMD dart server.dart
