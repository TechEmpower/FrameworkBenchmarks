FROM openjdk:8 AS builder
WORKDIR /http4s
COPY project project
COPY src src
COPY build.sbt build.sbt
COPY sbt sbt
RUN ./sbt assembly -batch && \
    mv target/scala-2.13/http4s-assembly-1.0.jar . && \
    rm -Rf target && \
    rm -Rf project/target && \
    rm -Rf ~/.sbt && \
    rm -Rf ~/.ivy2 && \
    rm -Rf /var/cache
FROM openjdk:alpine
WORKDIR /http4s
COPY --from=builder /http4s/http4s-assembly-1.0.jar /http4s/http4s-assembly-1.0.jar
CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:NewSize=1g", "-XX:MaxNewSize=1g", "-XX:InitialCodeCacheSize=256m", "-XX:ReservedCodeCacheSize=256m", "-XX:+UseParallelGC", "-XX:+UseNUMA", "-XX:-UseBiasedLocking", "-XX:+AlwaysPreTouch", "-jar", "http4s-assembly-1.0.jar", "tfb-database"]
