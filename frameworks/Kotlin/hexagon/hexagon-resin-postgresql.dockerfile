
#
# BUILD
#
FROM gradle:5.2.1-jdk11 AS gradle_build
USER root
WORKDIR /hexagon

COPY src src
COPY build.gradle build.gradle
COPY gradle.properties gradle.properties
RUN gradle --quiet --exclude-task test

#
# RUNTIME
#
FROM openjdk:11
ENV DBSTORE postgresql
ENV POSTGRESQL_DB_HOST tfb-database
ENV RESIN 4.0.58

RUN curl http://caucho.com/download/resin-$RESIN.tar.gz | tar xvz -C /opt
COPY --from=gradle_build /hexagon/build/libs/ROOT.war /opt/resin-$RESIN/webapps
ENTRYPOINT /opt/resin-$RESIN/bin/resin.sh console
