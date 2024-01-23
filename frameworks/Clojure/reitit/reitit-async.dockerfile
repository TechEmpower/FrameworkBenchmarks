FROM clojure:openjdk-11-lein-2.9.1
WORKDIR /reitit
COPY src src
COPY project.clj project.clj
RUN lein uberjar

EXPOSE 8080

CMD ["java", "-server", "-Xms2G", "-Xmx2G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-Dvertx.disableMetrics=true", "-Dvertx.threadChecks=false", "-Dvertx.disableContextTimings=true", "-Dvertx.disableTCCL=true", "-XX:+UseStringDeduplication", "-Djava.net.preferIPv4Stack=true", "-jar", "target/hello-reitit-standalone.jar", "async"]
