FROM adoptopenjdk/openjdk11:x86_64-alpine-jre-11.0.3_7
RUN apk add bash
WORKDIR /blaze
COPY project project
COPY src src
COPY build.sbt build.sbt
COPY sbt sbt
RUN ./sbt assembly -batch && \
    mv target/scala-2.12/blaze-assembly-1.0.jar . && \
    rm -Rf target && \
    rm -Rf project/target && \
    rm -Rf ~/.sbt && \
    rm -Rf ~/.ivy2 && \
    rm -Rf /var/cache
CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:NewSize=1g", "-XX:MaxNewSize=1g", "-XX:InitialCodeCacheSize=256m", "-XX:ReservedCodeCacheSize=256m", "-XX:+UseParallelGC", "-XX:+UseNUMA", "-XX:-UseBiasedLocking", "-XX:+AlwaysPreTouch", "-jar", "blaze-assembly-1.0.jar", "tfb-database"]
