FROM clojure:lein-2.8.1
WORKDIR /pedestal
COPY project.clj project.clj
COPY config config
COPY src src
COPY test test
RUN lein uberjar
CMD ["java", "-jar", "-Dio.pedestal.log.defaultMetricsRecorder=nil", "-Dio.pedestal.log.overrideLogger=nil", "target/pedestal-standalone.jar"]
