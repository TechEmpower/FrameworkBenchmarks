#
# BUILD
#
FROM docker.io/gradle:8.2.1-jdk17-alpine AS build
USER root
WORKDIR /hexagon

ADD . .
RUN gradle --quiet classes
RUN gradle --quiet -x test installDist

#
# RUNTIME
#
FROM docker.io/eclipse-temurin:20-jre-alpine
ARG PROJECT=hexagon_nima_pgclient

ENV POSTGRESQL_DB_HOST tfb-database
ENV JDK_JAVA_OPTIONS --enable-preview -XX:+AlwaysPreTouch -XX:+UseParallelGC -XX:+UseNUMA

COPY --from=build /hexagon/$PROJECT/build/install/$PROJECT /opt/$PROJECT

ENTRYPOINT [ "/opt/hexagon_nima_pgclient/bin/hexagon_nima_pgclient" ]
