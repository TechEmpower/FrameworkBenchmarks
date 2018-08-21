FROM google/dart:2.0

COPY ./ ./

RUN pub upgrade

CMD dart bin/main.dart --type=postgres