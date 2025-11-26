FROM clojure:lein as lein
WORKDIR /reitit
COPY project.clj project.clj
COPY src src
RUN lein uberjar

FROM amazoncorretto:25
WORKDIR /reitit
COPY --from=lein /reitit/target/hello-reitit-standalone.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseParallelGC", "-jar", "app.jar"]
