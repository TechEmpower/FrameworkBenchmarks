FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /fintrospect
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch
CMD ["java", "-Dcom.twitter.finagle.toggle.flag.overrides=com.twitter.http.UseNetty4=1.0", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "target/scala-2.12/techempower-benchmarks-fintrospect-assembly-1.0.jar"]
