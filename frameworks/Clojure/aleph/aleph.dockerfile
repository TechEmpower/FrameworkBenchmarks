FROM clojure:lein-2.8.1
WORKDIR /aleph
COPY project.clj project.clj
COPY src src
RUN lein uberjar
CMD ["java", "-server", "-Xmx2g", "-XX:+UseG1GC", "-XX:MaxGCPauseMillis=10", "-jar", "target/hello-aleph-standalone.jar"]
