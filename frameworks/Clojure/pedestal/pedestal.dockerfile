FROM techempower/leiningen-java8:0.1

ADD ./ /pedestal
WORKDIR /pedestal
RUN lein clean
RUN lein uberjar
CMD java \
    -jar \
    -D"io.pedestal.log.defaultMetricsRecorder=nil" \
    -D"io.pedestal.log.overrideLogger=nil" \
    target/pedestal-standalone.jar

