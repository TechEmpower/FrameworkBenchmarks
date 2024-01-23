#
# BUILD
#
FROM container-registry.oracle.com/graalvm/native-image:21-muslib-ol9 as build
USER root
WORKDIR /hexagon

ADD . .
RUN microdnf -y install findutils
RUN ./gradlew --quiet classes
RUN ./gradlew --quiet -x test hexagon_helidon_pgclient:nativeCompile

#
# RUNTIME
#
FROM scratch
ARG PROJECT=hexagon_helidon_pgclient

COPY --from=build /hexagon/$PROJECT/build/native/nativeCompile/$PROJECT /

ENTRYPOINT [ "/hexagon_helidon_pgclient" ]
