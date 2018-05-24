FROM clojure:lein-2.8.1
WORKDIR /pedestal
COPY config config
COPY src src
COPY test test
COPY project.clj project.clj
RUN lein uberjar
CMD ["java", "-jar", "-Dio.pedestal.log.defaultMetricsRecorder=nil", "-Dio.pedestal.log.overrideLogger=nil", "target/pedestal-standalone.jar"]
