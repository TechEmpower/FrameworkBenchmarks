
#
# BUILD
#
FROM gradle:5.5.1-jdk11 AS gradle_build
USER root
WORKDIR /hexagon

COPY src src
COPY build.gradle build.gradle
COPY gradle.properties gradle.properties
RUN gradle --quiet --exclude-task test

#
# RUNTIME
#
FROM openjdk:11.0.3-jre-stretch
ENV DBSTORE postgresql
ENV POSTGRESQL_DB_HOST tfb-database
ENV WEBENGINE jetty
ENV PROJECT hexagon

COPY --from=gradle_build /hexagon/build/install/$PROJECT /opt/$PROJECT
ENTRYPOINT /opt/$PROJECT/bin/$PROJECT
