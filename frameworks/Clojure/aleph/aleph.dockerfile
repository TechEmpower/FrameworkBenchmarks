FROM clojure:openjdk-17-lein-2.9.8
WORKDIR /aleph
COPY src src
COPY project.clj project.clj
RUN lein uberjar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "target/hello-aleph-standalone.jar"]
