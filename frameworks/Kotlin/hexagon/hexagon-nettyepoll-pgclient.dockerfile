#
# BUILD
#
FROM gradle:7.5.1-jdk17-alpine AS build
USER root
WORKDIR /hexagon

COPY src src
COPY build.gradle build.gradle
RUN gradle --quiet -x test

#
# RUNTIME
#
FROM eclipse-temurin:19-jre-alpine
ENV DBSTORE pg_client
ENV POSTGRESQL_DB_HOST tfb-database
ENV WEBENGINE netty_epoll
ENV PROJECT hexagon
ENV DISABLE_CHECKS true
ENV JDK_JAVA_OPTIONS --enable-preview -XX:+AlwaysPreTouch -XX:+UseParallelGC -XX:+UseNUMA

COPY --from=build /hexagon/build/install/$PROJECT /opt/$PROJECT

EXPOSE 9090

ENTRYPOINT /opt/$PROJECT/bin/$PROJECT
