FROM tfb/maven:latest
ADD ./ /netty
WORKDIR /netty
RUN mvn clean compile assembly:single
CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -jar target/netty-example-0.1-jar-with-dependencies.jar
