FROM clojure:lein as lein
WORKDIR /luminus
COPY env env
COPY project.clj project.clj
COPY resources resources
COPY src src
RUN lein uberjar

FROM amazoncorretto:25
WORKDIR /luminus
COPY --from=lein /luminus/target/hello.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseParallelGC", "-jar", "app.jar"]
