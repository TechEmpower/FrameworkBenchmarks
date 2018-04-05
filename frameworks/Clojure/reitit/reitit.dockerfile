FROM techempower/leiningen-java8:0.1

ADD ./ /reitit
WORKDIR /reitit
RUN lein clean
RUN lein uberjar
CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -jar target/hello-reitit-standalone.jar
