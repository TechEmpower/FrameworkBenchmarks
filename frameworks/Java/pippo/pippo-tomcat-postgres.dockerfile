FROM gradle:7.6.0-jdk17 as gradle
WORKDIR /pippo
COPY gradle gradle
COPY build.gradle build.gradle
COPY gradlew gradlew
COPY src src
RUN ./gradlew fatJar -x test

FROM eclipse-temurin:17.0.6_10-jre-jammy
WORKDIR /pippo
COPY --from=gradle /pippo/build/libs/pippo-all.jar app.jar

ARG BENCHMARK_ENV

ENV BENCHMARK_ENV=$BENCHMARK_ENV

EXPOSE 8080

CMD ["java", "-server", "-Xms4G", "-Xmx4G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-cp", "app.jar", "com.techempower.benchmark.pippo.benchmark.BenchmarkTomcat"]
