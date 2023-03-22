#
# BUILD
#
FROM gradle:8.0.2-jdk17-alpine AS build
USER root
WORKDIR /hexagon

ADD . .
RUN gradle --quiet compileRocker
RUN gradle --quiet -x test

#
# RUNTIME
#
FROM tomcat:10.1.2-jre17-temurin
ENV POSTGRESQL_DB_HOST tfb-database
ENV MODULE /hexagon/hexagon_tomcat_postgresql
ENV JDK_JAVA_OPTIONS -XX:+AlwaysPreTouch -XX:+UseParallelGC -XX:+UseNUMA

COPY --from=build $MODULE/build/libs/ROOT.war /usr/local/tomcat/webapps/ROOT.war
EXPOSE 8080
