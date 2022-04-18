FROM clojure:openjdk-17-lein-2.9.8
WORKDIR /aleph
COPY src src
COPY project.clj project.clj
RUN lein uberjar

EXPOSE 8080

CMD ["java", "-server", "-Xms2G", "-Xmx2G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+UseStringDeduplication", "-Djava.net.preferIPv4Stack=true", "-jar", "target/hello-aleph-standalone.jar"]
