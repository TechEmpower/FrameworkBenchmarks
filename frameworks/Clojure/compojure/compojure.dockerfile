FROM clojure:openjdk-11-lein-2.9.1 as lein
WORKDIR /compojure
COPY src src
COPY project.clj project.clj
RUN lein ring uberwar

FROM openjdk:11.0.3-jdk-stretch
WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.61.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=lein /compojure/target/hello-compojure-standalone.war webapps/ROOT.war
COPY resin.xml conf/resin.xml

EXPOSE 8080

CMD ["java", "-jar", "lib/resin.jar", "console"]
