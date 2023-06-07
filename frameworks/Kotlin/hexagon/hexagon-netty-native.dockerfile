#
# BUILD
#
FROM ghcr.io/graalvm/native-image:ol9-java17-22.3.2 as build
USER root
WORKDIR /hexagon

ADD . .
RUN microdnf -y install findutils
RUN ./gradlew --quiet classes
RUN ./gradlew --quiet -x test nativeCompile

#
# RUNTIME
#
FROM scratch
ARG PROJECT=hexagon_netty_postgresql

COPY --from=build /hexagon/$PROJECT/build/native/nativeCompile/$PROJECT /

ENTRYPOINT [ "/hexagon_netty_postgresql" ]
