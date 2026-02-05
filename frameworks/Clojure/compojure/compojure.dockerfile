FROM clojure:lein as lein
WORKDIR /compojure
COPY src src
COPY project.clj project.clj
RUN lein ring uberwar

FROM curlimages/curl:8.17.0 as resin-builder
USER root
WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.66.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=lein /compojure/target/hello-compojure-standalone.war webapps/ROOT.war

FROM amazoncorretto:25
WORKDIR /resin
COPY --from=resin-builder /resin .
COPY resin.xml conf/resin.xml
EXPOSE 8080
CMD ["java", "-XX:MaxRAMPercentage=70", "-Dclojure.compiler.direct-linking=true", "-jar", "lib/resin.jar", "console"]