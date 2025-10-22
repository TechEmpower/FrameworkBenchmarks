FROM clojure:lein as lein
WORKDIR /ring-http-exchange
COPY project.clj project.clj
COPY resources resources
COPY src src
RUN lein with-profile robaho uberjar

FROM openjdk:25-jdk-slim
WORKDIR /ring-http-exchange
COPY --from=lein /ring-http-exchange/target/ring-http-server-1.0.0-standalone.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseParallelGC", "-jar", "app.jar"]
