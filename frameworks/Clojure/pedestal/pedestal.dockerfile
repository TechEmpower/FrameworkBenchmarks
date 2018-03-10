FROM tfb/leiningen-java8:latest
ADD ./ /pedestal
WORKDIR /pedestal
RUN lein clean
RUN lein uberjar
CMD java \
    -jar \
    -D"io.pedestal.log.defaultMetricsRecorder=nil" \
    -D"io.pedestal.log.overrideLogger=nil" \
    target/pedestal-standalone.jar

