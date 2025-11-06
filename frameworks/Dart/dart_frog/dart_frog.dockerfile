FROM dart:3.8 AS builder

COPY . /app
WORKDIR /app

RUN dart pub global activate dart_frog_cli
RUN dart_frog build

RUN dart compile exe build/bin/server.dart -o build/bin/server

FROM scratch
COPY --from=builder /runtime/ /
COPY --from=builder /app/build/bin/server /app/build/bin/

EXPOSE 8080
CMD ["/app/build/bin/server"]
