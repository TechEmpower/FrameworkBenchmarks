FROM clojure:lein-2.8.1
WORKDIR /luminus
COPY project.clj project.clj
COPY Procfile Procfile
COPY env env
COPY resources resources
COPY src src
COPY test test
RUN lein uberjar
CMD ["java", "-server", "-jar", "target/hello.jar"]
