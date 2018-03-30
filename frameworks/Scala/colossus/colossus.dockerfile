FROM tfb/sbt:latest

ADD ./ /colossus
WORKDIR /colossus

RUN sbt assembly -batch

CMD java \
    -server \
    -Xms1g \
    -Xmx1g \
    -XX:NewSize=512m \
    -XX:MaxNewSize=512m \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -XX:+UseNUMA \
    -XX:-UseBiasedLocking \
    -XX:+AlwaysPreTouch \
    -jar target/scala-2.12/colossus-example-assembly-1.0.jar
