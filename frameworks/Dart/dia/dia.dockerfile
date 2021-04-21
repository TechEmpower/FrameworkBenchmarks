FROM google/dart:2.12

#ADD ./ /src
WORKDIR /src

COPY src/pubspec.yaml pubspec.yaml
COPY src/server.dart server.dart

RUN pub upgrade

EXPOSE 8080

CMD ["dart", "server.dart"]
