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
FROM tomcat:10.1.2-jre17-temurin
ENV DBSTORE postgresql
ENV POSTGRESQL_DB_HOST tfb-database
ENV DISABLE_CHECKS true
ENV JDK_JAVA_OPTIONS --enable-preview -XX:+AlwaysPreTouch -XX:+UseParallelGC -XX:+UseNUMA

COPY --from=build /hexagon/build/libs/ROOT.war /usr/local/tomcat/webapps/ROOT.war
EXPOSE 8080
