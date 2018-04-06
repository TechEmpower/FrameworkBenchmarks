FROM clojure:lein-2.8.1
WORKDIR /http-kit
COPY project.clj project.clj
COPY src src
RUN lein deps
RUN lein uberjar
CMD ["java", "-server", "-jar", "target/http-kit-standalone.jar"]
