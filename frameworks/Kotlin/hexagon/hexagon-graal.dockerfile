
FROM ghcr.io/graalvm/graalvm-ce:java11-21.1.0 as build
USER root
WORKDIR /build

ADD ./build/libs/*-all*.jar /build
RUN gu install native-image

# Generates a fallback image that requires a JDK for execution (the binary is smaller)
#RUN native-image -jar /build/hexagon_benchmark-all*.jar --force-fallback hexagon_benchmark

# ./hexagon_benchmark -Dcom.hexagonkt.noJmx=true -Duser.timezone=$TZ
# This creates a stand alone binary. Fails at start up (missing Logback and Pebble configuration)
RUN native-image -jar \
  /build/hexagon-all*.jar \
  --no-fallback \
  --static \
  --allow-incomplete-classpath \
  --report-unsupported-elements-at-runtime \
  --initialize-at-run-time=com.hexagonkt \
  -H:IncludeResources=".*" \
  -Djava.system.class.loader=com.oracle.svm.hosted.NativeImageSystemClassLoader \
  hexagon_benchmark

FROM scratch
COPY --from=build /build/hexagon_benchmark /
ENTRYPOINT /hexagon_benchmark
