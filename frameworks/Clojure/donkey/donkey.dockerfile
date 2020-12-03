FROM clojure:openjdk-11-lein-2.9.3 as lein
WORKDIR /donkey
COPY src src
COPY project.clj project.clj
RUN lein uberjar

FROM openjdk:11.0.3-jdk-slim
COPY --from=lein /donkey/target/hello-donkey-SNAPSHOT-standalone.jar  app.jar
CMD ["java", "-server", "-Xms2G", "-Xmx2G", "-Dlogging.level.root=OFF", "-XX:+UseStringDeduplication", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-Dvertx.disableMetrics=true", "-Dvertx.threadChecks=false", "-Dvertx.disableContextTimings=true", "-Dvertx.disableTCCL=true", "-Dvertx.disableH2c=true", "-Dvertx.disableWebsockets=true", "-Dvertx.disableHttpHeadersValidation=true", "-Dvertx.flashPolicyHandler=false", "-Djava.net.preferIPv4Stack=true", "-jar", "app.jar"]
