FROM clojure:lein-2.8.1
WORKDIR /aleph
COPY src src
COPY project.clj project.clj
RUN lein uberjar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "target/hello-aleph-standalone.jar"]
