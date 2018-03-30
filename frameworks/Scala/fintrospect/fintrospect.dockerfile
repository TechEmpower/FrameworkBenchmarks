FROM techempower/sbt:0.1

ADD ./ /fintrospect
WORKDIR /fintrospect

RUN sbt assembly -batch

CMD java \
    -Dcom.twitter.finagle.toggle.flag.overrides=com.twitter.http.UseNetty4=1.0 \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -XX:+AlwaysPreTouch \
    -jar target/scala-2.12/techempower-benchmarks-fintrospect-assembly-1.0.jar
