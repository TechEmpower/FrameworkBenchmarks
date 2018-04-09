FROM clojure:lein-2.8.1
WORKDIR /reitit
COPY project.clj project.clj
COPY src src
RUN lein uberjar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "target/hello-reitit-standalone.jar"]
