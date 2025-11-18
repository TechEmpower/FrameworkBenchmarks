FROM clojure:lein as lein
WORKDIR /aleph
COPY src src
COPY project.clj project.clj
RUN lein uberjar

FROM amazoncorretto:25

WORKDIR /aleph
COPY --from=lein /aleph/target/hello-aleph-standalone.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "--enable-native-access=ALL-UNNAMED", "-XX:+UseParallelGC", "-jar", "app.jar"]
