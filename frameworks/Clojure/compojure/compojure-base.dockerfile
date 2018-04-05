FROM techempower/leiningen-java8:0.1 as leiningen

ADD ./ /compojure
WORKDIR /compojure
RUN lein clean
RUN lein ring uberwar

FROM techempower/resin-java8:0.1

COPY --from=leiningen /compojure/target/hello-compojure-standalone.war ${RESIN_HOME}/webapps/ROOT.war
