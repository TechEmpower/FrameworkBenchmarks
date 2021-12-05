FROM openjdk:15 AS builder
WORKDIR /http4s
COPY blaze/project project
COPY blaze/src src
COPY blaze/build.sbt build.sbt
COPY sbt sbt
RUN ./sbt assembly -batch && \
    mv target/scala-2.13/http4s-assembly-1.0.jar . && \
    rm -Rf target && \
    rm -Rf project/target && \
    rm -Rf ~/.sbt && \
    rm -Rf ~/.ivy2 && \
    rm -Rf /var/cache
    
FROM openjdk:15
WORKDIR /http4s
COPY --from=builder /http4s/http4s-assembly-1.0.jar /http4s/http4s-assembly-1.0.jar

EXPOSE 8080

CMD java \
      -server \
      -Xms2g \
      -Xmx2g \
      -XX:+AlwaysPreTouch \
      -Dcats.effect.stackTracingMode=disabled \
      -jar \
      http4s-assembly-1.0.jar \
      tfb-database
