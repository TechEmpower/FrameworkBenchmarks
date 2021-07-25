#
# BUILD
#
FROM gradle:7.1-jdk11 AS gradle_build
USER root
WORKDIR /hexagon

COPY src src
COPY build.gradle build.gradle
RUN gradle --quiet --exclude-task test

#
# RUNTIME
#
FROM adoptopenjdk:11-jre-hotspot-bionic
ENV DBSTORE mongodb
ENV MONGODB_DB_HOST tfb-database
ENV WEBENGINE jetty
ENV PROJECT hexagon

COPY --from=gradle_build /hexagon/build/install/$PROJECT /opt/$PROJECT

EXPOSE 9090

ENTRYPOINT /opt/$PROJECT/bin/$PROJECT
