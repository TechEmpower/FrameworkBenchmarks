FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /firenio
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

EXPOSE 8080

CMD java                       \
    -server                    \
    -XX:+UseNUMA               \
    -XX:+UseParallelGC         \
    -XX:+AggressiveOpts        \
    -Dlite=false               \
    -Dcore=1                   \
    -Dframe=16                 \
    -DreadBuf=512              \
    -Dpool=true                \
    -Dlevel=1                  \
    -Dread=false               \
    -Depoll=true               \
    -Dnodelay=true             \
    -Dcachedurl=false          \
    -DunsafeBuf=true           \
    -jar /firenio/target/firenio-example-0.1-jar-with-dependencies.jar
