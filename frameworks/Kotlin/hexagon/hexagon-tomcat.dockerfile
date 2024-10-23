#
# BUILD
#
FROM docker.io/bellsoft/liberica-runtime-container:jdk-all-21-cds-musl AS build
USER root
WORKDIR /hexagon

ADD . .
RUN ./gradlew --quiet classes
RUN ./gradlew --quiet -x test war

#
# RUNTIME
#
FROM docker.io/tomcat:11.0.0-jre21-temurin-noble
ARG MODULE=/hexagon/hexagon_tomcat_postgresql

ENV POSTGRESQL_DB_HOST tfb-database
ENV JDK_JAVA_OPTIONS -XX:+AlwaysPreTouch -XX:+UseParallelGC -XX:+UseNUMA
ENV maximumPoolSize 300

COPY --from=build $MODULE/build/libs/ROOT.war /usr/local/tomcat/webapps/ROOT.war
