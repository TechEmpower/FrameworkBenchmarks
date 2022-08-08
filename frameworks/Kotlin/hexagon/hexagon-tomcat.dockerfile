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
FROM tomcat:10.1.0-jre17-temurin
ENV DBSTORE postgresql
ENV POSTGRESQL_DB_HOST tfb-database
ENV DISABLE_CHECKS true

COPY --from=gradle_build /hexagon/build/libs/ROOT.war /usr/local/tomcat/webapps/ROOT.war
EXPOSE 8080
