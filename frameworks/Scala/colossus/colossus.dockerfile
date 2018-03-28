FROM techempower/sbt:0.1

COPY ./ ./

RUN sbt 'oneJar' -batch

CMD java -server -Xms1g -Xmx1g -XX:NewSize=512m -XX:MaxNewSize=512m -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+UseNUMA -XX:-UseBiasedLocking -XX:+AlwaysPreTouch -jar target/scala-2.12/colossus*one-jar.jar
