FROM google/dart:2.0

COPY ./ ./

RUN pub get

RUN pub run build_runner build

CMD ANGEL_ENV=production dart bin/main.dart --type=mongo
