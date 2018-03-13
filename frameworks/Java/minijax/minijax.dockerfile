FROM tfb/maven:latest
ADD ./ /minijax
WORKDIR /minijax
RUN mvn clean package
CMD java \
    -server \
    -Xms512m \
    -Xmx2g \
    -XX:+AggressiveOpts \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -jar target/minijax-techempower-0.0.1.jar
