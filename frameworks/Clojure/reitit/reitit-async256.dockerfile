FROM clojure:lein-2.8.1
WORKDIR /reitit
COPY src src
COPY project.clj project.clj
RUN lein uberjar
CMD ["java", "-server", "-Xms2G", "-Xmx2G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-Dvertx.disableMetrics=true", "-Dvertx.threadChecks=false", "-Dvertx.disableContextTimings=true", "-Dvertx.disableTCCL=true", "-jar", "target/hello-reitit-standalone.jar", "async", "256"]
