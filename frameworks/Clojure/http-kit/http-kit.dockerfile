FROM clojure:lein-2.8.1 AS lein
WORKDIR /http-kit
COPY project.clj project.clj
RUN lein deps
RUN lein uberjar

FROM amazoncorretto:25
WORKDIR /http-kit
COPY --from=lein /http-kit/target/http-kit-standalone.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseParallelGC", "-Dsun.net.httpserver.nodelay=true", "-XX:MaxRAMPercentage=70", "-Dclojure.compiler.direct-linking=true", "-jar", "app.jar"]