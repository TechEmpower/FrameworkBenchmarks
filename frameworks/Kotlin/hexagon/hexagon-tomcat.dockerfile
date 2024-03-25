#
# BUILD
#
FROM docker.io/gradle:8.6-jdk21-alpine AS build
USER root
WORKDIR /hexagon

ADD . .
RUN gradle --quiet classes
RUN gradle --quiet -x test war

#
# RUNTIME
#
FROM docker.io/tomcat:10-jre21-temurin-jammy
ARG MODULE=/hexagon/hexagon_tomcat_postgresql

ENV POSTGRESQL_DB_HOST tfb-database
ENV JDK_JAVA_OPTIONS -XX:+AlwaysPreTouch -XX:+UseParallelGC -XX:+UseNUMA

COPY --from=build $MODULE/build/libs/ROOT.war /usr/local/tomcat/webapps/ROOT.war
