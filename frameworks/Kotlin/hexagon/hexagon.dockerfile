
FROM gradle:4.10.2-jdk11
USER root
WORKDIR /hexagon
COPY src src
COPY build.gradle build.gradle
COPY gradle.properties gradle.properties
COPY settings.gradle settings.gradle
RUN gradle --quiet --exclude-task test
ENV DBSTORE mongodb
ENV MONGODB_DB_HOST tfb-database
ENV WEBENGINE jetty
CMD ["build/install/hexagon/bin/hexagon"]
