#
# BUILD
#
FROM ghcr.io/graalvm/native-image-community:20-muslib-ol9 as build
USER root
WORKDIR /hexagon

ADD . .
RUN microdnf -y install findutils
RUN ./gradlew --quiet classes
RUN ./gradlew --quiet -x test hexagon_nima_postgresql:nativeCompile

#
# RUNTIME
#
FROM scratch
ARG PROJECT=hexagon_nima_postgresql

COPY --from=build /hexagon/$PROJECT/build/native/nativeCompile/$PROJECT /

ENTRYPOINT [ "/hexagon_nima_postgresql" ]
