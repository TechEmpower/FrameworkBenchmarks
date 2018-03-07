FROM tfb/sbt:latest

COPY ./ ./

RUN sbt 'oneJar' -batch

CMD java -Dcom.twitter.finagle.toggle.flag.overrides=com.twitter.http.UseNetty4=1.0 -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar target/scala-2.12/*fintrospect*one-jar.jar
