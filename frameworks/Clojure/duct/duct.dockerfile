FROM clojure:lein-2.8.1
WORKDIR /duct
COPY resources resources
COPY resources/hello/db-pg.edn resources/hello/db.edn
COPY resources/hello/server-jetty.edn resources/hello/server.edn
COPY src src
COPY Procfile Procfile
COPY project.clj project.clj
RUN lein uberjar

CMD ["java", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-server", "-jar", "target/hello-duct-standalone.jar"]
