FROM clojure:lein-2.8.1
WORKDIR /reitit
COPY src src
COPY project.clj project.clj
RUN lein uberjar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "target/hello-reitit-standalone.jar"]
