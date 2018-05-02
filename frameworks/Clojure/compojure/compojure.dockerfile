FROM clojure:lein-2.8.1 as lein
WORKDIR /compojure
COPY src src
COPY project.clj project.clj
RUN lein ring uberwar

FROM openjdk:8-jdk
WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.56.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=lein /compojure/target/hello-compojure-standalone.war webapps/ROOT.war
COPY resin.xml conf/resin.xml
CMD ["java", "-jar", "lib/resin.jar", "console"]
