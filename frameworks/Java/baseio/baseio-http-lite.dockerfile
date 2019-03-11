FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /baseio
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

CMD java                       \
    -server                    \
    -XX:+UseNUMA               \
    -XX:+UseParallelGC         \
    -XX:+AggressiveOpts        \
    -Dlite=true                \
    -Dcore=1                   \
    -Dframe=16                 \
    -DreadBuf=512              \
    -Dpool=true                \
    -Ddirect=true              \
    -Dinline=true              \
    -Dlevel=1                  \
    -Dread=false               \
    -Depoll=true               \
    -DunsafeBuf=true           \
    -jar /baseio/target/baseio-example-0.1-jar-with-dependencies.jar