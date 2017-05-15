rm -r ./target; lein clean
lein uberjar
java -jar -D"io.pedestal.log.defaultMetricsRecorder=nil" -D"io.pedestal.log.overrideLogger=nil" target/pedestal-standalone.jar

