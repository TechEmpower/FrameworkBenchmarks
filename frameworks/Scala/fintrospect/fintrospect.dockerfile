FROM hseeberger/scala-sbt:8u222_1.3.5_2.13.1
WORKDIR /fintrospect
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch

EXPOSE 9000

CMD ["java", "-Dcom.twitter.finagle.toggle.flag.overrides=com.twitter.http.UseNetty4=1.0", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "target/scala-2.13/techempower-benchmarks-fintrospect-assembly-1.0.jar"]
