FROM clojure:lein-2.8.1
WORKDIR /duct
COPY resources resources
COPY resources/hello/db-mongo.edn resources/hello/db.edn
COPY resources/hello/server-aleph.edn resources/hello/server.edn
COPY src src
COPY Procfile Procfile
COPY project.clj project.clj
RUN lein uberjar

EXPOSE 3000

CMD ["java", "-server", "-jar", "target/hello-duct-standalone.jar"]
