FROM techempower/maven:0.1

ADD ./ /jetty
WORKDIR /jetty
RUN mvn -P servlet clean compile assembly:single
CMD java \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -jar target/jetty-example-0.1-jar-with-dependencies.jar
