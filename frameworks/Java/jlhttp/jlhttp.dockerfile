FROM tfb/maven:latest
ADD ./ /jlhttp
WORKDIR /jlhttp
RUN mvn clean compile assembly:single
CMD java \
    -server \
    -Xss256k \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -jar target/jlhttp-1.0-jar-with-dependencies.jar
