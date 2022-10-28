#
# BUILD
#
FROM gradle:7.5.0-jdk17-alpine AS gradle_build
USER root
WORKDIR /hexagon

COPY src src
COPY build.gradle build.gradle
RUN gradle --quiet

#
# RUNTIME
#
FROM eclipse-temurin:17-jre-alpine
ENV DBSTORE postgresql
ENV POSTGRESQL_DB_HOST tfb-database
ENV WEBENGINE netty_epoll
ENV PROJECT hexagon
ENV DISABLE_CHECKS true

COPY --from=gradle_build /hexagon/build/install/$PROJECT /opt/$PROJECT

EXPOSE 9090

ENTRYPOINT /opt/$PROJECT/bin/$PROJECT
