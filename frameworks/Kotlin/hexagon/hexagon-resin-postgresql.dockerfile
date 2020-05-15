#
# BUILD
#
FROM gradle:6.4-jdk11 AS gradle_build
USER root
WORKDIR /hexagon

COPY src src
COPY build.gradle build.gradle
COPY gradle.properties gradle.properties
RUN gradle --quiet --exclude-task test

#
# RUNTIME
#
FROM openjdk:11.0.7-jre-slim
ENV DBSTORE postgresql
ENV POSTGRESQL_DB_HOST tfb-database
ENV RESIN http://caucho.com/download/resin-4.0.64.tar.gz

WORKDIR /resin
RUN curl -sL $RESIN | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=gradle_build /hexagon/build/libs/ROOT.war webapps/ROOT.war
COPY resin.xml conf/resin.xml
CMD ["java", "-jar", "lib/resin.jar", "console"]
