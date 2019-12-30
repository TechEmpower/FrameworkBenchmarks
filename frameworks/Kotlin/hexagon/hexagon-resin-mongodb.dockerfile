
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
ENV DBSTORE mongodb
ENV MONGODB_DB_HOST tfb-database
ENV RESIN http://caucho.com/download/resin-4.0.61.tar.gz

WORKDIR /resin
RUN curl -sL $RESIN | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=gradle_build /hexagon/build/libs/ROOT.war webapps/ROOT.war
COPY resin.xml conf/resin.xml
CMD ["java", "-jar", "lib/resin.jar", "console"]
