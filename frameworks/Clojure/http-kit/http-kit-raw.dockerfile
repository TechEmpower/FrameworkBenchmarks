FROM clojure:lein-2.8.1
WORKDIR /http-kit
COPY src src
COPY project.clj project.clj
RUN lein deps
RUN lein uberjar

EXPOSE 8080

CMD ["java", "-server", "-jar", "target/http-kit-standalone.jar"]
