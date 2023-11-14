#
# BUILD
#
FROM ghcr.io/graalvm/native-image-community:21-muslib-ol9 as build
USER root
WORKDIR /hexagon

ADD . .
RUN microdnf -y install findutils
RUN ./gradlew --quiet classes
RUN ./gradlew --quiet -x test hexagon_jetty_postgresql:nativeCompile

#
# RUNTIME
#
FROM scratch
ARG PROJECT=hexagon_jetty_postgresql

COPY --from=build /hexagon/$PROJECT/build/native/nativeCompile/$PROJECT /

ENTRYPOINT [ "/hexagon_jetty_postgresql" ]
