FROM tfb/leiningen-java8:latest as leiningen
ADD ./ /compojure
WORKDIR /compojure
RUN lein clean
RUN lein ring uberwar

FROM tfb/resin-java8:latest
COPY --from=leiningen /compojure/target/hello-compojure-standalone.war ${RESIN_HOME}/webapps/ROOT.war
