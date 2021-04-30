FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /isocket
COPY pom.xml pom.xml
COPY src src
RUN mvn clean compile assembly:single -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /isocket
COPY --from=maven /isocket/target/isocket-nio-benchmark-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-cp", "app.jar", "cn.ibaijia.tfb.HttpBootstrap"]
