FROM debian:buster as builder

RUN apt-get update && apt-get install -y curl gnupg && \
  echo "deb https://dl.bintray.com/sbt/debian /" > /etc/apt/sources.list.d/sbt.list && \
  curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add - && \
  curl -sL https://nginx.org/keys/nginx_signing.key | apt-key add - && \
  echo "deb https://packages.nginx.org/unit/debian/ buster unit" > /etc/apt/sources.list.d/unit.list && \
  echo "deb-src https://packages.nginx.org/unit/debian/ buster unit" >> /etc/apt/sources.list.d/unit.list && \
  apt-get update && apt-get install -y clang unit-dev libuv1-dev openjdk-11-jdk sbt && \
  apt-get purge -y gnupg

WORKDIR /workdir

COPY . .

RUN sbt nativeLink

FROM nginx/unit:1.22.0-minimal

RUN apt-get update && apt-get install -y libuv1

COPY /config.json /docker-entrypoint.d/
COPY --from=builder /workdir/target/scala-2.13/workdir-out /app/example

EXPOSE 8080
