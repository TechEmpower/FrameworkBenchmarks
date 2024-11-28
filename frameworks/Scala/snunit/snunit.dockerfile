FROM debian:bookworm-slim as builder

RUN apt-get update && apt-get install -y curl gnupg && \
  echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list  && \
  curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add - && \
  curl -sL https://nginx.org/keys/nginx_signing.key | apt-key add - && \
  echo "deb https://packages.nginx.org/unit/debian/ bookworm unit" > /etc/apt/sources.list.d/unit.list && \
  echo "deb-src https://packages.nginx.org/unit/debian/ bookworm unit" >> /etc/apt/sources.list.d/unit.list && \
  apt-get update && apt-get install -y clang unit-dev=1.33* openjdk-17-jdk sbt=1.10.5 && \
  apt-get purge -y gnupg

WORKDIR /workdir

COPY . .

RUN sbt nativeLink

FROM unit:1.33.0-minimal

COPY /config.sh /docker-entrypoint.d/
COPY --from=builder /workdir/target/scala-3.5.2/workdir /app/example

EXPOSE 8080
