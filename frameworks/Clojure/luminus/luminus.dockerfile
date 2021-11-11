FROM clojure:lein-2.8.1
WORKDIR /luminus
COPY env env
COPY resources resources
COPY src src
COPY test test
COPY Procfile Procfile
COPY project.clj project.clj
RUN lein uberjar

EXPOSE 3000

CMD ["java", "-server", "-jar", "target/hello.jar"]
