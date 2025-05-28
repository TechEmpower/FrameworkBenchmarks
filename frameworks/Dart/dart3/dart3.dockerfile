
FROM dart:3.8 AS builder

COPY . /app
WORKDIR /app
RUN mkdir build
RUN dart compile exe ./bin/server.dart -o build/server

FROM scratch
COPY --from=builder /runtime/ /
COPY --from=builder /app/build /bin

EXPOSE 8080
CMD ["server"]
