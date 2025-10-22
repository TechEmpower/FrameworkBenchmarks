FROM gradle:8.7.0-jdk21
USER root
WORKDIR /http4k
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY core core
COPY core-jdbc core-jdbc
COPY jettyloom-jdbc jettyloom-jdbc
RUN gradle --quiet --no-daemon jettyloom-jdbc:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "--enable-preview", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "jettyloom-jdbc/build/libs/http4k-benchmark.jar"]
