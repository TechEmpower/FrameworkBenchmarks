FROM dart:3.4.4

WORKDIR /app
COPY pubspec.yaml pubspec.yaml
COPY server.dart server.dart

RUN dart pub upgrade

EXPOSE 8080

CMD ["dart", "server.dart"]