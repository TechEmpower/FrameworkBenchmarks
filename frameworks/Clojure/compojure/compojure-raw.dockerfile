FROM clojure:lein-2.8.1 as lein
WORKDIR /compojure
COPY project.clj project.clj
COPY src src
RUN lein ring uberwar

FROM techempower/resin-java8:0.1
COPY --from=lein /compojure/target/hello-compojure-standalone.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
