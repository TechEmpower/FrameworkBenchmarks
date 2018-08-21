FROM google/dart:2.0

COPY ./ ./

RUN pub get

CMD ANGEL_ENV=production dart bin/main.dart --type=mongo