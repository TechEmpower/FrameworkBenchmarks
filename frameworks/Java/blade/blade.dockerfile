FROM tfb/maven:latest
ADD ./ /blade
WORKDIR /blade
RUN mvn clean package
CMD java \
    -Xms2G \
    -Xmx2G \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -jar target/dist/hello-blade-1.0.0-BUILD-SNAPSHOT/hello-blade-1.0.0-BUILD-SNAPSHOT.jar
