#
# BUILD
#
FROM docker.io/bellsoft/liberica-runtime-container:jdk-all-24-cds-musl AS build
USER root
WORKDIR /hexagon

ADD . .
RUN ./gradlew --quiet classes
RUN ./gradlew --quiet -x test installDist

#
# RUNTIME
#
FROM docker.io/bellsoft/liberica-runtime-container:jre-24-musl
ARG PROJECT=hexagon_helidon_postgresql

ENV POSTGRESQL_DB_HOST tfb-database
ENV JDK_JAVA_OPTIONS --enable-preview -XX:+AlwaysPreTouch -XX:+UseParallelGC -XX:+UseNUMA
ENV maximumPoolSize 300

COPY --from=build /hexagon/$PROJECT/build/install/$PROJECT /opt/$PROJECT

ENTRYPOINT [ "/opt/hexagon_helidon_postgresql/bin/hexagon_helidon_postgresql" ]
