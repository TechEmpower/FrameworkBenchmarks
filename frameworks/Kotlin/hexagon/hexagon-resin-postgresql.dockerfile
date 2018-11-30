
#
# BUILD
#
FROM gradle:5.0.0-jdk11 AS gradle_build
USER root
WORKDIR /hexagon

COPY src src
COPY build.gradle build.gradle
COPY gradle.properties gradle.properties

RUN gradle --quiet --exclude-task test

#
# RESIN
#
FROM openjdk:11 AS resin_runtime
USER root
ENV RESIN 4.0.58
RUN curl http://caucho.com/download/resin-$RESIN.tar.gz | tar xvz -C /opt

#
# RUNTIME
#
FROM resin_runtime
USER root

ENV RESIN 4.0.58
ENV DBSTORE postgresql
ENV POSTGRESQL_DB_HOST tfb-database

COPY --from=gradle_build /hexagon/build/libs/ROOT.war /opt/resin-$RESIN/webapps

WORKDIR /opt/resin-$RESIN
EXPOSE 8080
ENTRYPOINT /opt/resin-$RESIN/bin/resin.sh console
